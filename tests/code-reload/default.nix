import ../make-test.nix ({ lib, system, ... }:

let
  testClient = pkgs: pkgs.headcounter.buildErlang rec {
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

  argsFile = pkgs: let
    client = testClient pkgs;
    deps = [ client ] ++ client.recursiveErlangDeps;
  in pkgs.writeText "testclient.args" ''
    ${lib.concatMapStringsSep "\n" (dep: "-pa ${dep.appDir}/ebin") deps}
    -sname test@client
    -setcookie testclient
    -noinput
    -s testclient start
  '';

  nodes = {
    server = {
      networking.extraHosts = "127.0.0.1 server";
      headcounter.services.mongooseim = {
        enable = true;
        settings = {
          hosts = [ "server" ];
          modules.register.options.ip_access = [];
          extraConfig = {
            access.multi = [
              { extuple = [
                  { atom = "c2s"; } [
                    { tuple = [ { atom = "allow"; } { atom = "all"; } ]; }
                  ]
                ];
              }
              { extuple = [
                  { atom = "local"; } [
                    { tuple = [ { atom = "allow"; } { atom = "all"; } ]; }
                  ]
                ];
              }
              { extuple = [
                  { atom = "register"; } [
                    { tuple = [ { atom = "allow"; } { atom = "all"; } ]; }
                  ]
                ];
              }
            ];
            registration_timeout.atom = "infinity";
          };
        };
      };
    };

    client = { pkgs, ... }: {
      networking.extraHosts = "127.0.0.1 client";
      environment.systemPackages = [ pkgs.erlang ];
      systemd.services.testclient = {
        description = "Test Client";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" "fs.target" "keys.target" ];

        environment.HOME = testClient pkgs;
        serviceConfig.ExecStart = "@${pkgs.erlang}/bin/erl testclient"
                                + " -args_file ${argsFile pkgs}";
      };
    };
  };

  newServerConfig = {
    headcounter.services.mongooseim.settings.extraConfig = {
      access.multi = [
        { extuple = [
            { atom = "c2s"; } [
              { tuple = [ { atom = "deny"; } { atom = "all"; } ]; }
            ]
          ];
        }
      ];
    };
  };

  newServerCode = { pkgs, ... }: {
    headcounter.services.mongooseim.package = let
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
        # XXX: Shouldn't need to include common.nix again!
        imports = [ ../../common.nix nodes.server ] ++ configurations;
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
        'erl_call -sname test@client -c testclient '.
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

    subtest "stop server", sub {
      $server->succeed("systemctl stop mongooseim");
      assertTestClient("check_connections", "not_connected_anymore");
    };
  '';
})
