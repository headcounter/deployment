import ../make-test.nix ({ pkgs, lib, system, ... }:

let
  testClient = pkgs.headcounter.buildErlang rec {
    name = "test-client";
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

  newServer = {
    services.headcounter.mongooseim.settings.extraConfig = ''
      {access, c2s, [{deny, all}]}.
    '';
  };

  newServerBuild = let
    inherit (import <nixpkgs/nixos/lib/build-vms.nix> {
      inherit system;
    }) buildVirtualNetwork;

    newNodes = nodes // {
      server = {
        imports = [ nodes.server newServer ];
      };
    };
  in (buildVirtualNetwork newNodes).server.config.system.build.toplevel;

in {
  name = "code-reload";

  inherit nodes;

  testScript = ''
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

    startAll;
    $server->waitForUnit("mongooseim.service");
    $client->waitForUnit("testclient.service");

    assertTestClient("ping", "pong");
    assertTestClient("register", "register_done");
    assertTestClient("login", "logged_in");
    assertTestClient("communicate", "great_communication");

    $server->sleep(10); # Let the server gather uptime
    my $old_uptime = sendTestClientCommand("get_uptime");

    $server->succeed("${newServerBuild}/bin/switch-to-configuration test");

    assertTestClient("check_connections", "still_connected");

    my $new_uptime = sendTestClientCommand("get_uptime");

    assertTestClient("login", "login_failure");

    die "old server uptime is $old_uptime seconds, ".
        "but new uptime is just $new_uptime seconds, ".
        "so the server has restarted in-between!"
        if $old_uptime > $new_uptime;
  '';
})
