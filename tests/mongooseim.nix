import <nixpkgs/nixos/tests/make-test.nix> ({ pkgs, ... }:

let
  localPkgs = import ../pkgs {
    inherit pkgs;
  };

  server1 = "server1";
  server2 = "server2";

  nodeName1 = "mongooseim@${server1}";
  nodeName2 = "mongooseim@${server2}";
  cookie = "mongooseim"; # XXX! Also remember: It's an atom!

  testLibs = with localPkgs; let
    mkEbin = d: "${d.appDir}/ebin";
    recEbin = map mkEbin mongooseimTests.recursiveErlangDeps;
  in pkgs.lib.singleton (mkEbin mongooseimTests) ++ recEbin;

  testRunner = with localPkgs; pkgs.lib.concatStringsSep " " ([
    "${pkgs.erlang}/bin/erl"
    "-sname test@client"
    "-noinput"
    "-setcookie mongooseim"
    "-pa ${mongooseimTests}/tests ${pkgs.lib.concatStringsSep " " testLibs}"
    "-s run_common_test main test=full spec=default.spec"
  ]);

  escalusConfig = pkgs.writeText "test.config" ''
    {ejabberd_node, '${nodeName1}'}.
    {ejabberd2_node, '${nodeName2}'}.
    {ejabberd_cookie, ${cookie}}.
    {ejabberd_domain, <<"${server1}">>}.
    {ejabberd_addr, <<"${server1}">>}.
    {ejabberd_secondary_domain, <<"${server2}">>}.
    {ejabberd_reloaded_domain, <<"sogndal">>}.
    {ejabberd_metrics_rest_port, 5280}.
    {ejabberd_string_format, bin}.

    {escalus_user_db, xmpp}.

    {escalus_server, <<"${server1}">>}.
    {escalus_server2, <<"${server2}">>}.

    {escalus_users, [
      {alice, [
        {username, <<"alicE">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"makota">>},
        {compression, <<"zlib">>}
      ]},
      {bob, [
        {username, <<"bOb">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"makrolika">>},
        {ssl, optional}
      ]},
      {carol, [
        {username, <<"carol">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"jinglebells">>},
        {transport, bosh},
        {path, <<"/http-bind">>},
        {port, 5280}
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
        {transport, ws},
        {port, 5280},
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
      {oldie, [
        {username, <<"oldie">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"legacy">>},
        {transport, ws},
        {port, 5280},
        {wspath, <<"/ws-xmpp">>},
        {wslegacy, true}
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
        {ssl, required}
      ]},
      {astrid, [
        {username, <<"astrid">>},
        {server, <<"sogndal">>},
        {host, <<"${server2}">>},
        {password, <<"doctor">>}
      ]}
    ]}.

    {escalus_server2_users, [
      {alice2, [
        {username, <<"alice">>},
        {server, <<"${server2}">>},
        {host, <<"${server2}">>},
        {port, 5222},
        {password, <<"makota2">>}
      ]},
      {bob2, [
        {username, <<"bob">>},
        {server, <<"${server2}">>},
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

    {mam, [
      {skipped_configurations, [ca]}
    ]}.
  '';

  mkConfig = serverName: {
    hosts = [ serverName "${serverName}.bis" "anonymous.${serverName}" ];
    s2s.filterDefaultPolicy = "allow";

    listeners = [ # FIXME: Unique port/module and maybe loaOf?
      { port = 5222;
        module = "ejabberd_c2s";
        options.access.atom = "c2s";
        options.shaper.atom = "c2s_shaper";
        options.max_stanza_size = 65536;
      }
      { port = 5280;
        module = "ejabberd_cowboy";
        options.num_acceptors = 10;
        options.max_connections = 1024;
        options.modules = [
          { tuple = [
              server1
              "/api"
              { atom = "mongoose_api"; }
              { handlers = [
                  { atom = "mongoose_api_metrics"; }
                  { atom = "mongoose_api_users"; }
                ];
              }
            ];
          }
          { tuple = ["_" "/http-bind" { atom = "mod_bosh"; }]; }
          { tuple = ["_" "/ws-xmpp"   { atom = "mod_websockets"; }]; }
        ];
      }
      { port = 5269;
        module = "ejabberd_s2s_in";
        options.shaper.atom = "s2s_shaper";
        options.max_stanza_size = 131072;
      }
    ];

    modules = {
      bosh.enable = true;
      offline.enable = true;
      offline.options.access_max_user_messages = {
        atom = "max_user_offline_messages";
      };
      register.options.ip_access = [];
      websockets.enable = true;
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
in {
  nodes = {
    server1 = { config, pkgs, ... }: {
      imports = import ../modules/module-list.nix;
      services.headcounter.mongooseim = {
        enable = true;
        settings = mkConfig server1;
      };
      networking.firewall.enable = false;
    };

    server2 = { config, pkgs, ... }: {
      imports = import ../modules/module-list.nix;
      services.headcounter.mongooseim = {
        enable = true;
        settings = mkConfig server2;
      };
      networking.firewall.enable = false;
    };

    client = {};
  };

  testScript = ''
    startAll;
    $server1->waitForUnit("mongooseim.service");
    $server2->waitForUnit("mongooseim.service");

    $client->succeed('cp -Lr "${localPkgs.mongooseimTests}/tests" .');
    $client->succeed('cp "${localPkgs.mongooseimTests}/etc/vcard.config" .');
    $client->succeed('cp "${localPkgs.mongooseimTests}/etc/default.spec" .');
    $client->succeed('cp "${escalusConfig}" test.config');

    $client->succeed('sed -i '.
                     '-e \'s/mongooseim@localhost/${nodeName1}/g\' '.
                     '-e \'s/localhost/${server1}/g\' '.
                     'tests/*.erl vcard.config');

    # Disable mongooseimctl test (Too SysV'ish right now)
    $client->succeed('sed -i -e \'/ejabberdctl_SUITE/d\' default.spec');

    my $clientip = $server2->succeed('getent hosts client | cut -d" " -f1');
    chomp $clientip;
    $client->succeed("sed -i -e 's/127\\.0\\.0\\.1/$clientip/' ".
                     "tests/sic_SUITE.erl");

    $client->succeed('sed -i -e \'/wait_for_stanza/s/10000/&0/\' '.
                     'tests/s2s_SUITE.erl');

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
        die;
      }
    });
  '';
})
