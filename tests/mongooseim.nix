{ pkgs, ... }:

let
  localPkgs = import ../pkgs {
    inherit pkgs;
  };

  server1 = "server1";
  server2 = "server2";

  nodeName = "ejabberd@server1";
  cookie = "ejabberd"; # XXX! Also remember: It's an atom!

  testRunner = pkgs.lib.concatStringsSep " " ([
    "${pkgs.erlang}/bin/erl"
    "-sname test@client"
    "-noinput"
    "-setcookie ejabberd"
    "-pa ${localPkgs.mongooseimTests}/tests"
    "${localPkgs.mongooseimTests}/ebin"
    "${localPkgs.mongooseimTests}/deps/*/ebin" # */
    "-s run_common_test ct"
  ]);

  escalusConfig = pkgs.writeText "test.config" ''
    {ejabberd_node, '${nodeName}'}.
    {ejabberd_cookie, ${cookie}}.
    {ejabberd_domain, <<"${server1}">>}.
    {ejabberd_secondary_domain, <<"${server2}">>}.
    {ejabberd_metrics_rest_port, 5281}.

    {escalus_users, [
      {alice, [
        {username, <<"alice">>},
        {server, <<"${server1}">>},
        {host, <<"${server1}">>},
        {password, <<"makota">>},
        {compression, <<"zlib">>}
      ]},
      {bob, [
        {username, <<"bob">>},
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
        {port, 5288},
        {wspath, <<"/ws-xmpp">>}
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

  mkConfig = serverName: pkgs.writeText "ejabberd.cfg" ''
    {loglevel, 3}.

    {hosts, ["${serverName}",
             "${serverName}.bis",
             "anonymous.${serverName}"]}.

    {auth_method, internal}.

    {host_config, "anonymous.${serverName}", [
      {auth_method, [anonymous]},
      {allow_multiple_connections, true},
      {anonymous_protocol, both}
    ]}.

    {listen, [
      {5280, mod_bosh, [{num_acceptors, 10}]},
      {5222, ejabberd_c2s, [{access, c2s}, {shaper, c2s_shaper},
                            {max_stanza_size, 65536}]},
      {{5288, ws}, mod_websockets, [{host, "${serverName}"},
                                    {prefix, "/ws-xmpp"}]},
      {5269, ejabberd_s2s_in, [{shaper, s2s_shaper},
                               {max_stanza_size, 131072}]}
    ]}.

    {s2s_default_policy, allow}.
    {outgoing_s2s_port, 5269}.
    {sm_backend, {mnesia, []}}.

    {shaper, normal, {maxrate, 1000}}.
    {shaper, fast, {maxrate, 50000}}.
    {max_fsm_queue, 1000}.

    {acl, local, {user_regexp, ""}}.

    {access, max_user_sessions, [{10, all}]}.
    {access, max_user_offline_messages, [{5000, admin}, {100, all}]}.
    {access, local, [{allow, local}]}.
    {access, c2s, [{deny, blocked},
                   {allow, all}]}.
    {access, c2s_shaper, [{fast, all}]}.
    {access, s2s_shaper, [{fast, all}]}.
    {access, muc_admin, [{allow, admin}]}.
    {access, muc_create, [{allow, all}]}.
    {access, muc, [{allow, all}]}.

    {access, register, [{allow, all}]}.
    {registration_timeout, infinity}.

    {language, "en"}.

    {modules, [
      {mod_adhoc, []},
      {mod_disco, []},
      {mod_last, []},
      {mod_muc, [{host, "muc.${serverName}"}, {access, muc},
                 {access_create, muc_create}]},
      {mod_muc_log, [{outdir, "/tmp/muclogs"}, {access_log, muc}]},
      {mod_offline, [{access_max_user_messages, max_user_offline_messages}]},
      {mod_privacy, []},
      {mod_private, []},
      {mod_register, [{welcome_message, {""}}, {ip_access, []},
                      {access, register}]},
      {mod_roster, []},
      {mod_sic, []},
      {mod_vcard, [{allow_return_all, true}, {search_all_hosts, true}]},
      {mod_metrics, [{port, 8081}]}
    ]}.
  '';
in {
  nodes = {
    server1 = { config, pkgs, ... }: {
      imports = [ ../modules/services/mongooseim.nix ];
      services.headcounter.mongooseim = {
        enable = true;
        configFile = mkConfig server1;
      };
    };

    server2 = { config, pkgs, ... }: {
      imports = [ ../modules/services/mongooseim.nix ];
      services.headcounter.mongooseim = {
        enable = true;
        configFile = mkConfig server2;
      };
    };

    client = {};
  };

  testScript = ''
    startAll;
    foreach my $waitport (5222, 5269, 5280, 5288) {
      $server1->waitForOpenPort($waitport);
      $server2->waitForOpenPort($waitport);
    }

    $client->succeed('cp -Lr "${localPkgs.mongooseimTests}/tests" .');
    $client->succeed('cp -Lr ${localPkgs.mongooseimTests}/deps/* tests/');
    $client->succeed('cp "${localPkgs.mongooseimTests}/etc/vcard.config" .');
    $client->succeed('cp "${escalusConfig}" test.config');
    $client->succeed('sed -i '.
                     '-e \'s/ejabberd@localhost/${nodeName}/g\' '.
                     '-e \'s/localhost/${server1}/g\' '.
                     'tests/*.erl vcard.config');

    $client->succeed('${pkgs.erlang}/bin/erl -noinput '.
                     '-setcookie ${cookie} -sname ejabberd@client '.
                     '-eval "pong = net_adm:ping(\'${nodeName}\'), '.
                            'erlang:halt()"');

    my $testCmd = 'mkdir -p ct_report && ${testRunner} >&2';

    $client->nest("running test suite: $testCmd", sub {
      my $rval = ($client->execute_($testCmd))[0];
      my $out = $ENV{'out'};

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
}
