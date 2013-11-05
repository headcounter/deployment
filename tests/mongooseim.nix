{ pkgs, ... }:

let
  mkConfig = serverName: pkgs.writeText "ejabberd.cfg" ''
    {loglevel, 5}.
    {hosts, ["${serverName}"]}.

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
    {auth_method, internal}.

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
    {access, muc_create, [{allow, local}]}.
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
        configFile = mkConfig "server1";
      };
    };

    server2 = { config, pkgs, ... }: {
      imports = [ ../modules/services/mongooseim.nix ];
      services.headcounter.mongooseim = {
        enable = true;
        configFile = mkConfig "server2";
      };
    };

    client1 = { config, pkgs, ... }: {
      environment.systemPackages = pkgs.lib.singleton (import ../pkgs {
        inherit pkgs;
      }).testXMPP;
    };

    client2 = { config, pkgs, ... }: {
      environment.systemPackages = pkgs.lib.singleton (import ../pkgs {
        inherit pkgs;
      }).testXMPP;
    };
  };

  testScript = ''
    startAll;
    $server1->waitForUnit("mongooseim.service");
    $server2->waitForUnit("mongooseim.service");

    $client1->waitUntilSucceeds("netcat -z server1 5222");
    $client2->waitUntilSucceeds("netcat -z server2 5222");

    $client1->succeed('test-xmpp testuser1@server1 register');
    $client2->succeed('test-xmpp testuser2@server2 register');
  '';
}
