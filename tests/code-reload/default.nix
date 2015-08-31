import ../make-test.nix ({ pkgs, lib, system, ... }:

let
  testClient = pkgs.headcounter.buildErlang rec {
    name = "testclient";
    version = "1.0";

    erlangDeps = with pkgs.headcounter.erlangPackages; [
      escalus
    ];

    src = pkgs.stdenv.mkDerivation {
      name = "test-client-src";

      buildCommand = ''
        mkdir -p "$out/src"

        cat > "$out/src/testclient.app.src" <<EOF
        {application, testclient, [
          {description, "Test client for hot code reloading"},
          {vsn, "${version}"},
          {modules, []},
          {registered, [testclient]},
          {applications, [kernel, stdlib]}
        ]}.
        EOF

        cat > "$out/rebar.config" <<EOF
        {deps, [{escalus, ".*", none}]}.
        EOF

        cat "${./testclient.erl}" > "$out/src/testclient.erl"
      '';
    };
  };

  deps = [ testClient ] ++ testClient.recursiveErlangDeps;

  argsFile = pkgs.writeText "testclient.args" ''
    ${lib.concatMapStringsSep "\n" (dep: "-pa ${dep.appDir}/ebin") deps}
    -sname test@client
    -setcookie testclient
    -noinput
    -s testclient start
  '';

  nodes = {
    server = {
      imports = [ ../../common.nix ];
      networking.extraHosts = "127.0.0.1 server";
      services.headcounter.mongooseim = {
        enable = true;
        settings = {
          hosts = [ "server" ];
          modules.register.options.ip_access = [];
          extraConfig = ''
            {access, c2s, [{allow, all}]}.
            {access, local, [{allow, all}]}.
            {access, register, [{allow, all}]}.
            {registration_timeout, infinity}.
          '';
        };
      };
    };

    client = {
      imports = [ ../../common.nix ];
      networking.extraHosts = "127.0.0.1 client";
      systemd.services.testclient = {
        description = "Test Client";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" "fs.target" "keys.target" ];

        environment.HOME = testClient;
        serviceConfig.ExecStart = "@${pkgs.erlang}/bin/erl testclient"
                                + " -args_file ${argsFile}";
      };
    };
  };

  newServerConfig = {
    services.headcounter.mongooseim.settings.extraConfig = ''
      {access, c2s, [{deny, all}]}.
    '';
  };

  newServerCode = {
    services.headcounter.mongooseim.package = let
      patched = pkgs.headcounter.mongooseim.overrideDerivation (drv: {
        postPatch = (drv.postPatch or "") + ''
          sed -i -e 's!<<"Pong">>!<<"Pang">>!' \
            apps/ejabberd/src/mod_adhoc.erl
        '';
      });
    in patched;
  };

  buildNewServer = configurations: let
    inherit (import <nixpkgs/nixos/lib/build-vms.nix> {
      inherit system;
    }) buildVirtualNetwork;

    newNodes = nodes // {
      server = {
        imports = [ nodes.server ] ++ configurations;
      };
    };
  in (buildVirtualNetwork newNodes).server.config.system.build.toplevel;

  newServerConfigBuild = buildNewServer [ newServerConfig ];
  revertedServerConfigBuild = buildNewServer [];
  newServerCodeBuild = buildNewServer [ newServerCode ];

in {
  name = "code-reload";

  inherit nodes;

  testScript = let
    switchToServer = build: ''
      $server->succeed("${build}/bin/switch-to-configuration test");
    '';

  in ''
    sub sendTestClientCommand {
      return $client->succeed(
        '${pkgs.erlang}/bin/erl_call -sname test@client -c testclient '.
        '-a "gen_server call [testclient, '.$_[0].', 60000]"'
      );
    }

    sub assertTestClient {
      my ($command, $expect) = @_;
      my $result = sendTestClientCommand($command);
      die "Expected $expect but got $result instead" if $result ne $expect;
    }

    sub assertUptime {
      my ($old, $new) = @_;
      die "old server uptime is $old seconds, ".
          "but new uptime is just $new seconds, ".
          "so the server has restarted in-between!"
          if $old > $new;
    }

    startAll;
    $server->waitForUnit("mongooseim.service");
    $client->waitForUnit("testclient.service");

    my ($old_uptime,
        $new_conf_uptime,
        $reverted_config_uptime,
        $new_code_uptime);

    subtest "initial version", sub {
      assertTestClient("ping", "pong");
      assertTestClient("register", "register_done");
      assertTestClient("login", "logged_in");
      assertTestClient("adhoc_ping", "pong");
      assertTestClient("communicate", "great_communication");

      $server->sleep(10); # Let the server gather uptime
      $old_uptime = sendTestClientCommand("get_uptime");
    };

    subtest "change configuration", sub {
      ${switchToServer newServerConfigBuild}

      assertTestClient("check_connections", "still_connected");

      $new_conf_uptime = sendTestClientCommand("get_uptime");

      assertTestClient("login", "login_failure");

      assertUptime($old_uptime, $new_conf_uptime);
    };

    subtest "revert configuration", sub {
      ${switchToServer revertedServerConfigBuild}

      assertTestClient("login", "logged_in");
      assertTestClient("adhoc_ping", "pong");

      $reverted_config_uptime = sendTestClientCommand("get_uptime");
      assertUptime($new_conf_uptime, $reverted_config_uptime);
    };

    subtest "change code", sub {
      ${switchToServer newServerCodeBuild}

      assertTestClient("check_connections", "still_connected");
      assertTestClient("adhoc_ping", "pang");

      $new_code_uptime = sendTestClientCommand("get_uptime");

      assertUptime($reverted_config_uptime, $new_code_uptime);
    };
  '';
})
