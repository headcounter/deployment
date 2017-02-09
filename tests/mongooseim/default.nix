import ../make-test.nix ({ pkgs, lib, ... }:


let
  server1 = "server1";
  server2 = "server2";

  nodeName1 = "mongooseim@${server1}";
  nodeName2 = "mongooseim@${server2}";
  cookie = "mongooseim";

  testLibs = with pkgs.headcounter; let
    mkEbin = d: "${d.appDir}/ebin";
    recEbin = map mkEbin mongooseimTests.recursiveErlangDeps;
  in lib.singleton (mkEbin mongooseimTests) ++ recEbin;

  testRunner = with pkgs.headcounter; lib.concatStringsSep " " ([
    "${pkgs.erlang}/bin/erl"
    "-sname test@client"
    "-noinput"
    "-setcookie ${cookie}"
    "-pa ${mongooseimTests}/tests ${lib.concatStringsSep " " testLibs}"
    "-s run_common_test main test=full spec=default.spec"
  ]);

  mkRosterTemplate = serverName: { pkgs, ... }: {
    environment.etc."mongooseim/roster.template" = {
      source = pkgs.runCommand "roster.template" {
        input = "${pkgs.headcounter.mongooseimTests}/etc/roster.template";
      } "sed -e 's!localhost!${serverName}!g' \"$input\" > \"$out\"";
    };
  };

  escalusConfig = pkgs.writeText "test.config" ''
    {ejabberd_node, '${nodeName1}'}.
    {ejabberd_cookie, ${cookie}}.
    {ejabberd_string_format, bin}.

    {hosts, [
      {mim, [
        {node, '${nodeName1}'},
        {domain, <<"${server1}">>},
        {cluster, mim},
        {secondary_domain, <<"${server1}.bis">>},
        {reloaded_domain, <<"sogndal">>},
        {metrics_rest_port, 5280}
      ]},
      {mim2, [
        {node, '${nodeName2}'},
        {domain, <<"${server2}">>},
        {cluster, mim}
      ]}
    ]}.

    {escalus_server, <<"${server1}">>}.
    {escalus_server2, <<"${server2}">>}.
    {escalus_user_db, {module, escalus_ejabberd}}.
    {escalus_xmpp_server, escalus_mongooseim}.

    {escalus_users, [
      {alice, [
        {username, <<"alicE">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"matygrysa">>}
      ]},
      {bob, [
        {username, <<"bOb">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"makrolika">>}
      ]},
      {bob_altpass, [
        {username, <<"bOb">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"niemakrolika">>}
      ]},
      {carol, [
        {username, <<"carol">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"jinglebells">>},
        {transport, escalus_bosh},
        {path, <<"/http-bind">>},
        {port, 5280}
      ]},
      {carol_s, [
        {username, <<"carol_s">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"jinglebells_s">>},
        {transport, escalus_bosh},
        {ssl, true},
        {path, <<"/http-bind">>},
        {port, 5285}
      ]},
      {kate, [
        {username, <<"kate">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"makrowe;p">>}
      ]},
      {mike, [
        {username, <<"mike">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"nicniema">>}
      ]},
      {geralt, [
        {username, <<"geralt">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"witcher">>},
        {transport, escalus_ws},
        {port, 5280},
        {wspath, <<"/ws-xmpp">>}
      ]},
      {geralt_s, [
        {username, <<"geralt_s">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"witcher_s">>},
        {transport, escalus_ws},
        {ssl, true},
        {port, 5285},
        {wspath, <<"/ws-xmpp">>}
      ]},
      {hacker, [
        {username, <<"hacker">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"bringdowntheserver">>},
        {compression, <<"zlib">>},
        {port, 5223}
      ]},
      {admin, [
        {username, <<"admin">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"bruce_almighty">>}
      ]},
      {secure_joe, [
        {username, <<"secure_joe">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"break_me">>},
        {compression, <<"zlib">>},
        {starttls, required}
      ]},
      {astrid, [
        {username, <<"astrid">>},
        {server, <<"sogndal">>},
        {host, <<"${server2}">>},
        {password, <<"doctor">>}
      ]},
      {alice2, [
        {username, <<"alice">>},
        {server, <<"${server2}">>},
        {host, <<"${server2}">>},
        {port, 5222},
        {password, <<"makota2">>}
      ]},
      {bob2, [
        {username, <<"bob">>},
        {server, <<109,105,99,104,97,197,130>>},
        {host, <<"${server2}">>},
        {port, 5222},
        {password, <<"makota3">>}
      ]},
      {clusterguy, [
        {username, <<"clusterguy">>},
        {server, <<"${server2}">>},
        {host, <<"${server2}">>},
        {password, <<"distributionftw">>},
        {port, 5222}
      ]},
      {clusterbuddy, [
        {username, <<"clusterbuddy">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"wasssssssup">>},
        {port, 5232}
      ]}
    ]}.

    {escalus_anon_users, [
      {jon, [
        {username, <<"jon">>},
        {server, <<"anonymous.${server1}">>},
        {host, <<"${server1}">>},
        {auth_method, <<"SASL-ANON">>}
      ]}
    ]}.
  '';

  mkConfig = serverName: nodes: let
    certs = import ../../ssl/snakeoil.nix serverName;
    privKeyFile = pkgs.writeText "priv.pem" certs.privateKey;
    pubKeyFile = pkgs.writeText "pub.pem" certs.publicKey;
  in {
    hosts = [ serverName "${serverName}.bis" "anonymous.${serverName}" ];
    s2s.filterDefaultPolicy = "allow";
    s2s.outgoing.staticHosts = lib.mapAttrs (lib.const (eval: {
      ipAddress = eval.config.networking.primaryIPAddress;
    })) nodes;

    listeners = [ # FIXME: Unique port/module and maybe loaOf?
      { port = 5222;
        module = "ejabberd_c2s";
        options.access.atom = "c2s";
        options.shaper.atom = "c2s_shaper";
        options.max_stanza_size = 65536;
      }
      { port = 5280;
        http.enable = true;
        http.modules = [
          { path = "/http-bind";
            handler = "mod_bosh";
          }
          { path = "/ws-xmpp";
            handler = "mod_websockets";
            options.ejabberd_service = {
              access.atom = "all";
              shaper_rule.atom = "fast";
              password = "secret";
            };
          }
        ];
        options.num_acceptors = 10;
        options.max_connections = 1024;
      }
      { port = 5285;
        http.enable = true;
        http.modules = [
          { path = "/http-bind";
            handler = "mod_bosh";
          }
          { path = "/ws-xmpp";
            handler = "mod_websockets";
          }
        ];
        options.num_acceptors = 10;
        options.max_connections = 1024;
        options.ssl.certfile = toString pubKeyFile;
        options.ssl.keyfile = toString privKeyFile;
        options.ssl.password = "";
      }
      { port = 8088;
        http.enable = true;
        http.modules = lib.singleton {
          path = "/api";
          handler = "mongoose_api_admin";
        };
        options.num_acceptors = 10;
        options.max_connections = 1024;
      }
      { port = 8089;
        http.enable = true;
        http.modules = [
          { path = "/api/sse";
            handler = "lasse_handler";
            options.mongoose_client_api_sse.flag = true;
          }
          { path = "/api/messages/[:with]";
            handler = "mongoose_client_api_messages";
          }
          { path = "/api/rooms/[:id]";
            handler = "mongoose_client_api_rooms";
          }
          { path = "/api/rooms/:id/users/[:user]";
            handler = "mongoose_client_api_rooms_users";
          }
          { path = "/api/rooms/[:id]/messages";
            handler = "mongoose_client_api_rooms_messages";
          }
        ];
        options.num_acceptors = 10;
        options.max_connections = 1024;
        options.compress = true;
        options.ssl.certfile = toString pubKeyFile;
        options.ssl.keyfile = toString privKeyFile;
        options.ssl.password = "";
      }
      { port = 5269;
        module = "ejabberd_s2s_in";
        options.shaper.atom = "s2s_shaper";
        options.max_stanza_size = 131072;
      }
    ];

    modules = {
      amp.enable = true;
      bosh.enable = true;
      commands.enable = true;
      offline.options.access_max_user_messages = {
        atom = "max_user_offline_messages";
      };
      register.options.ip_access = [];
      register.options.access.atom = "register";
      register.options.registration_watchers = [ "admin@${serverName}" ];
      mam_meta.enable = true;
      mam_meta.options.backend.atom = "odbc";
      muc_commands.enable = true;
      muc_light_commands.enable = true;
    };

    odbc = {
      type = "pgsql";
      password = "test";
    };

    extraConfig = ''
      {host_config, "anonymous.${serverName}", [
        {auth_method, [anonymous]},
        {allow_multiple_connections, true},
        {anonymous_protocol, both}
      ]}.

      {shaper, normal, {maxrate, 1000}}.
      {shaper, fast, {maxrate, 50000}}.
      {max_fsm_queue, 1000}.

      {acl, local, {user_regexp, ""}}.

      {access, max_user_sessions, [{10, all}]}.
      {access, max_user_offline_messages, [{5000, admin}, {100, all}]}.
      {access, local, [{allow, local}]}.
      {access, c2s, [{deny, blocked},
                     {allow, all}]}.
      {access, c2s_shaper, [{none, admin},
                            {normal, all}]}.
      {access, s2s_shaper, [{fast, all}]}.
      {access, muc_admin, [{allow, admin}]}.
      {access, muc_create, [{allow, all}]}.
      {access, muc, [{allow, all}]}.

      {access, register, [{allow, all}]}.
      {registration_timeout, infinity}.

      {language, "en"}.
    '';
  };

  storageConfig = {
    services.postgresql.enable = true;

    # Default pool sizes:
    #
    #   mod_mam_odbc_async_pool_writer:       32
    #   mod_mam_muc_odbc_async_pool_writer:   32
    #   per virtual host:                     10
    #
    # We have 3 virtual hosts, so we have 30 connections plus one MUC pool and
    # two private message pools, which leads to 126 (32 * 3 + 10 * 3).
    #
    # Another 3 connections are reserved for the superuser so we need
    # max_connections to be 129.
    services.postgresql.extraConfig = "max_connections = 129";

    services.postgresql.initialScript = pkgs.writeText "initial.sql" ''
      CREATE ROLE mongooseim WITH LOGIN PASSWORD 'test';
      CREATE DATABASE mongooseim;
      \c mongooseim
      \i ${pkgs.headcounter.mongooseim.mainAppDir}/priv/pg.sql
      GRANT USAGE ON SCHEMA public TO mongooseim;
      GRANT SELECT, INSERT, UPDATE, DELETE
         ON ALL TABLES IN SCHEMA public TO mongooseim;
      GRANT USAGE ON ALL SEQUENCES IN SCHEMA public TO mongooseim;
    '';
  };

  inherit (import ./lib.nix { inherit pkgs lib; }) runInCtl checkListeners;

in {
  name = "mongooseim";

  nodes = {
    server1 = { config, nodes, pkgs, ... }: {
      imports = [ ../../common.nix (mkRosterTemplate server1) storageConfig ];
      virtualisation.memorySize = 2048;
      headcounter.services.epmd.addresses = [ "0.0.0.0" ];
      headcounter.services.mongooseim = {
        enable = true;
        nodeIp = null;
        inherit cookie;
        settings = mkConfig server1 nodes;
      };
    };

    server2 = { config, nodes, pkgs, ... }: {
      imports = [ ../../common.nix (mkRosterTemplate server2) storageConfig ];
      virtualisation.memorySize = 2048;
      headcounter.services.epmd.addresses = [ "0.0.0.0" ];
      headcounter.services.mongooseim = {
        enable = true;
        nodeIp = null;
        inherit cookie;
        settings = mkConfig server2 nodes;
      };
    };

    client = {
      imports = [ ../../common.nix (mkRosterTemplate server1) ];
      virtualisation.memorySize = 1024;
      headcounter.programs.mongooseimctl = {
        enable = true;
        inherit cookie;
        destNodeName = nodeName1;
      };
    };
  };

  testScript = { nodes, ... }: with pkgs.headcounter; ''
    startAll;
    $server1->waitForUnit("mongooseim.service");
    $server2->waitForUnit("mongooseim.service");

    ${runInCtl "server1" (checkListeners nodes.server1)}
    ${runInCtl "server2" (checkListeners nodes.server2)}

    $client->succeed('cp -Lr "${mongooseimTests}/tests" .');
    $client->succeed('cp "${mongooseimTests}/etc/default.spec" .');
    $client->succeed('cp "${escalusConfig}" test.config');

    $client->succeed(
      'find tests -mindepth 1 -maxdepth 1 '.
      '-path tests/mod_http_notification_SUITE.erl -o '.
      '-name \'*.erl\' -exec sed -i '.
      '-e \'s/mongooseim@localhost/${nodeName1}/g\' '.
      '-e \'s/localhost/${server1}/g\' '.
      '{} +'
    );

    my $clientip = '${nodes.client.config.networking.primaryIPAddress}';
    $client->succeed("sed -i -e 's/127\\.0\\.0\\.1/$clientip/' ".
                     "tests/sic_SUITE.erl");

    $client->succeed('sed -i -e \'/wait_for_stanza/s/10000/&0/\' '.
                     'tests/s2s_SUITE.erl');

    $client->succeed('sed -i -e \'s/localhost/client/g\' '.
                     'tests/mod_http_notification_SUITE.erl');

    $client->succeed(
      'sed -i -e \'/^ *TemplatePath *=/s!=.*!= "${
        "/etc/mongooseim/roster.template"
      }",!\' -e \'s,FileName *= *",&/tmp/,\' '.
      ' -e \'s/@lo\\.\\*/@se.*/\' '.
      ' -e \'s/@loc\\.\\*t2/@ser.*r2/\' -e \'s/@localho+/@serve+/\' '.
      ' -e \'s/loc\\.\\*st/ser.*r1/\'   -e \'s/\\.\\*host/.*ver1/\' '.
      'tests/ejabberdctl_SUITE.erl'
    );

    $client->succeed('${pkgs.erlang}/bin/erl -noinput '.
                     '-setcookie ${cookie} -sname mongooseim@client '.
                     '-eval "pong = net_adm:ping(\'${nodeName1}\'), '.
                            'erlang:halt()"');

    $client->succeed('${pkgs.erlang}/bin/erl -noinput '.
                     '-setcookie ${cookie} -sname mongooseim@client '.
                     '-eval "pong = net_adm:ping(\'${nodeName2}\'), '.
                            'erlang:halt()"');

    my $testCmd = 'mkdir -p ct_report && ${testRunner} >&2';

    $client->nest("running test suite: $testCmd", sub {
      my $rval = ($client->execute_($testCmd))[0];
      my $out = $ENV{'out'};

      my $rawugly = $client->succeed(
        'find ct_report -name \'*@*\' -print | '.
        'xargs -I{} sh -c \'mv "{}" "$(echo "{}" | '.
        'tr @ _)" && basename "{}"\' '
      );
      chomp $rawugly;
      my @uglynames = split "\n", $rawugly;
      foreach my $ugly (@uglynames) {
        $client->succeed('find ct_report -type f -exec '.
                         "sed -i -e 's|$ugly|".($ugly =~ s/\@/_/gr)."|' {} +");
      }

      $client->succeed('tar cf /tmp/xchg/ct_report.tar ct_report && sync');
      system("tar xf vm-state-client/xchg/ct_report.tar -C '$out'");

      open HYDRA_PRODUCTS, ">>$out/nix-support/hydra-build-products";
      print HYDRA_PRODUCTS "report ct-tests $out/ct_report\n";
      close HYDRA_PRODUCTS;

      my @summaries = <$out/ct_report/ct_run.*/*.logs/run.*/suite.summary>;
      my @stats;
      foreach my $stat (@summaries) {
        open STAT, $stat;
        my @row = split(/\D+/, <STAT>);
        $stats[$_] += $row[$_ + 1] for (0 .. ($#row - 1));
        close STAT
      }

      my $total = $stats[0] + $stats[1];
      my $skip = $stats[2] + $stats[3];
      $client->log("$stats[0] out of $total tests succeeded ($skip skipped)");

      if ($rval != 0 || $stats[0] < $total) {
        $client->log("$stats[1] tests failed (test runner ".
                     "exited with exit code $rval)");
        open TOUCH_FAILED, ">>$out/nix-support/failed";
        close TOUCH_FAILED;
      }
    });
  '';
})
