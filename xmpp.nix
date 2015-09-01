{ pkgs, lib, config, ... }:

with lib;

let
  ciphers = concatStringsSep ":" [
    "ECDHE-RSA-AES256-GCM-SHA384"
    "ECDHE-ECDSA-AES256-GCM-SHA384"
    "ECDHE-RSA-AES256-SHA384"
    "ECDHE-ECDSA-AES256-SHA384"
    "DHE-DSS-AES256-GCM-SHA384"
    "DHE-RSA-AES256-GCM-SHA384"
    "DHE-RSA-AES256-SHA256"
    "DHE-DSS-AES256-SHA256"
    "ECDH-RSA-AES256-GCM-SHA384"
    "ECDH-ECDSA-AES256-GCM-SHA384"
    "ECDH-RSA-AES256-SHA384"
    "ECDH-ECDSA-AES256-SHA384"
    "ECDHE-RSA-AES128-GCM-SHA256"
    "ECDHE-ECDSA-AES128-GCM-SHA256"
    "ECDHE-RSA-AES128-SHA256"
    "ECDHE-ECDSA-AES128-SHA256"
    "DHE-DSS-AES128-GCM-SHA256"
    "DHE-RSA-AES128-GCM-SHA256"
    "DHE-RSA-AES128-SHA256"
    "DHE-DSS-AES128-SHA256"
    "ECDH-RSA-AES128-GCM-SHA256"
    "ECDH-ECDSA-AES128-GCM-SHA256"
    "ECDH-RSA-AES128-SHA256"
    "ECDH-ECDSA-AES128-SHA256"
    "@STRENGTH"
  ];

in {
  # XXX: Refactor me!
  config.users.extraUsers.mongoose.extraGroups = [ "keys" ];

  config.services.headcounter.mongooseim = {
    enable = true;
    settings = {
      hosts = [
        "headcounter.org"
        "aszlig.net"
        "no-icq.org"
        "noicq.org"
        #"anonymous.headcounter.org"
      ];

      s2s.filterDefaultPolicy = "allow";
      s2s.useStartTLS = "required";
      s2s.outgoing.port = 5269;
      s2s.outgoing.addressFamilies = [ "ipv6" "ipv4" ];

      listeners = flatten (mapAttrsToList (name: domain: let
        mkAddr = module: attrs: [
          (attrs // { inherit module; address = domain.ipv4; })
          (attrs // { inherit module; address = domain.ipv6; })
        ];

        mkC2S = isLegacy: mkAddr "ejabberd_c2s" ({
          port = if isLegacy then 5223 else 5222;
          options = (optionalAttrs (domain.ssl.privateKey != null) {
            certfile = domain.ssl.privateKey.path;
          }) // {
            access.atom = "c2s";
            max_stanza_size = 65536;
            shaper = "c2s_shaper";
            inherit ciphers;
          } // (if isLegacy then {
            tls.flag = true;
          } else {
            starttls.flag = true;
            starttls_required.flag = true;
          });
        });

        c2s = mkC2S true ++ mkC2S false;

        bosh = mkAddr "mod_bosh" {
          port = 5280;
          options.tls.flag = true;
        };

        s2s = mkAddr "ejabberd_s2s_in" {
          port = 5269;
          options = {
            max_stanza_size = 131072;
            shaper = "s2s_shaper";
          } // optionalAttrs (domain.ssl.privateKey != null) {
            certfile = domain.ssl.privateKey.path;
          };
        };
      in c2s ++ s2s ++ bosh) config.headcounter.vhosts) /* ++ [
        FIXME: ejabberd_service doesn't exist anymore in MongooseIM!

        { port = 5280;
          address = "127.0.0.1";
          module = "mod_bosh";
          options.access.atom = "public";
        }
        { port = 5555;
          address = "127.0.0.1";
          module = "ejabberd_service";
          options.access.atom = "public";
          options.hosts = singleton "icq.headcounter.org";
          options.password = "TODO";
        }
      ] */;

      modules = {
        /* FIXME: Not supported yet in MongooseIM
        mod_announce.enable = true;
        mod_announce.options.access.atom = "announce";

        caps.enable = true;
        configure.enable = true;

        irc.enable = true;
        irc.options = {
          access.atom = "public";
          host = "irc.headcounter.org";
        };

        stats.enable = true;
        stats.options.access.atom = "admin";

        shared_roster.enable = true;

        pubsub.enable = true;
        pubsub.options = {
          access_createnode.atom = "pubsub_createnode";
          last_item_cache = false;
          plugins = [ "flat" "hometree" "pep" ];
        };

        time.enable = true;
        time.options.access.atom = public;

        version.enable = true;
        version.options = {
          access.atom = "public";
          show_os = false;
        };

        proxy65.enable = true;
        proxy65.options = {
          access.atom = "public";
          shaper.atom = "ft_shaper";
        };
        */

        adhoc.enable = true;
        adhoc.options.access.atom = "public";

        register.enable = true;
        register.options = {
          access.atom = "register";
          welcome_message.tuple = [
            "Welcome!"
            "Welcome to the Headcounter Jabber Service. "
            "For information about this Network, please visit "
            "https://jabber.headcounter.org/"
          ];
        };

        roster.enable = true;
        roster.options = {
          access.atom = "public";
          versioning = true;
          store_current_id = false;
        };

        privacy.enable = true;
        privacy.options.access.atom = "public";

        admin_extra.enable = true;

        disco.enable = true;
        disco.options = {
          access.atom = "public";
          server_info = let
            mkInfo = { modules ? { atom = "all"; }, field, value }: {
              tuple = [ modules field (singleton value) ];
            };
          in [
            (mkInfo {
              field = "abuse-addresses";
              value = "mailto:abuse@headcounter.org";
            })
            (mkInfo {
              modules = singleton "mod_muc";
              field = "Web chatroom logs";
              value = "https://jabber.headcounter.org/chatlogs/";
            })
            (mkInfo {
              modules = singleton "mod_disco";
              field = "feedback-addresses";
              value = "xmpp:main@conference.headcounter.org";
            })
            (mkInfo {
              modules = [ "mod_disco" "mod_vcard" ];
              field = "admin-addresses";
              value = "xmpp:aszlig@aszlig.net";
            })
          ];
          extra_domains = map (base: "${base}.headcounter.org") [
            "icq" "irc" "pubsub" "vjud"
            # TODO: routing!
          ];
        };

        vcard.enable = true;
        vcard.options = {
          access.atom = "public";
          search = false;
          host = "vjud.headcounter.org";
        };

        offline.enable = true;
        offline.options = {
          access.atom = "public";
          access_max_user_messages.atom = "max_user_offline_messages";
        };

        private.enable = true;
        private.options.access.atom = "public";

        bosh.enable = true;
        bosh.options.port = 5280; # TODO: TLS and whatnot?

        muc.enable = true;
        muc.options = {
          access.atom = "muc";
          access_create.atom = "muc";
          access_persistent.atom = "muc";
          access_admin.atom = "muc_admin";
          host = "conference.headcounter.org";
        };

        muc_log.enable = true;
        muc_log.options = {
          access_log.atom = "muc_wallops";
          spam_prevention = true;
          outdir = "/var/www/chatlogs"; # TODO!
        };

        ping.enable = true;
        ping.options = {
          send_pings = true;
          ping_interval = 240;
        };

        last.enable = true;
        last.options.access.atom = "public";
      };

      extraConfig = ''
        % administrative
        {acl, admin, {user, "TODO", "aszlig.net"}}.
        {acl, wallops, {user, "TODO", "aszlig.net"}}.

        % Local users:
        {acl, local, {user_regexp, ""}}.

        {acl, anonymous, {server, "anonymous.headcounter.org"}}.

        % we don't allow too short names!
        {acl, weirdnames, {user_glob, "?"}}.
        {acl, weirdnames, {user_glob, "??"}}.

        % Everybody can create pubsub nodes
        {access, pubsub_createnode, [{allow, all}]}.

        % Only admins can use configuration interface:
        {access, configure, [{allow, admin}]}.

        % Every username can be registered via in-band registration:
        {access, register, [{deny, weirdnames}, {deny, all}]}.

        {access, public, [{allow, all}]}.

        % Only admins can send announcement messages:
        {access, announce, [{allow, admin},
                            {allow, wallops}]}.

        % Only non-blocked users can use c2s connections:
        {access, c2s, [{deny, blocked},
                       {deny, anonymous},
                       {allow, all}]}.

        % all security-aware users can use poll/bind
        {access, pollers, [{deny, blocked},
                           {allow, all}]}.

        % shaper stuff
        {shaper, slow, {maxrate, 500}}.
        {shaper, normal, {maxrate, 5000}}.
        {shaper, fast, {maxrate, 50000}}.
        {shaper, ultrafast, {maxrate, 500000}}.

        % limits
        {access, max_user_sessions, [{10, all}]}.
        {access, max_user_offline_messages, [{5000, admin}, {200, all}]}.

        % For all users except admins use "normal" shaper
        {access, c2s_shaper, [{none, admin},
                              {none, wallops},
                              {normal, all}]}.

        % For all users except admins use "ultrafast" shaper
        {access, ft_shaper, [{none, admin},
                             {none, wallops},
                             {ultrafast, all}]}.

        % For all S2S connections use "fast" shaper
        {access, s2s_shaper, [{fast, all}]}.

        % Admins of this server are also admins of MUC service:
        {access, muc_admin, [{allow, admin}]}.

        % Restricted MUC admin
        {access, muc_wallops, [{allow, admin},
                               {allow, wallops}]}.

        {access, muc, [{allow, all}]}.

        % This rule allows access only for local users:
        {access, local, [{allow, local}]}.

        %%
        %% watchdog_admins: If an ejabberd process consumes too much memory,
        %% send live notifications to those Jabber accounts.
        %%
        {watchdog_admins, []}.

        % See issue #13!
        % {host_config, "anonymous.headcounter.org", [
        %   {auth_method, anonymous},
        %   {allow_multiple_connections, true},
        %   {anonymous_protocol, both}
        % ]}.

        % Default language for server messages
        {language, "en"}.

        {s2s_ciphers, "${ciphers}"}.

        % S2S certificates
        ${concatStrings (mapAttrsToList (name: domain: ''
        {domain_certfile, "${domain.fqdn}", "${domain.ssl.privateKey.path}"}.
        '') (filterAttrs (_: d: d.fqdn != null) config.headcounter.vhosts))}
      '';
    };
  };
}
