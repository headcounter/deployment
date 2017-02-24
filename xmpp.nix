{ pkgs, lib, hclib, config, ... }:

with lib;

let
  tlsv1 = [
    "ECDH-ECDSA-AES256-SHA"
    "ECDH-RSA-AES256-SHA"
    "ECDHE-ECDSA-AES256-SHA"
    "ECDHE-RSA-AES256-SHA"
  ];

  legacy = [
    "DHE-RSA-AES256-SHA"
    "DHE-DSS-AES256-SHA"
  ];

  aes128fs = [
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
  ];

  aes256fs = [
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
  ];

  mkCiphers = clist: concatStringsSep ":" (clist ++ [ "@STRENGTH" ]);

  clientCiphers = mkCiphers (aes128fs ++ aes256fs ++ tlsv1 ++ legacy);
  serverCiphers = mkCiphers aes256fs;

in {
  # XXX: Refactor me!
  config.users.extraUsers.mongoose.extraGroups = [ "keys" ];

  config.headcounter.services.mongooseim = {
    enable = true;
    settings = {
      hosts = [
        "headcounter.org"
        "aszlig.net"
        "no-icq.org"
        "noicq.org"
        "torservers.net"
        #"anonymous.headcounter.org"
      ];

      auth.options.password_format.atom = "scram";
      auth.options.scram_iterations = 65536;
      auth.mechanisms = [ "scram" ];

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
            ciphers = clientCiphers;
            protocol_options = [ "no_sslv2" "no_sslv3" ];
          } // (if isLegacy then {
            tls.flag = true;
          } else {
            starttls.flag = true;
            starttls_required.flag = true;
          });
        });

        c2s = mkC2S true ++ mkC2S false;

        cowboy = mkAddr "ejabberd_cowboy" {
          port = 5280;
          options.tls.flag = true;
          options.modules = [
            { tuple = ["_" "/http-bind" { atom = "mod_bosh"; }]; }
          ];
        };

        s2s = mkAddr "ejabberd_s2s_in" {
          port = 5269;
          options = {
            max_stanza_size = 131072;
            shaper = "s2s_shaper";
            ciphers = serverCiphers;
            protocol_options = [ "no_sslv2" "no_sslv3" ];
          } // optionalAttrs (domain.ssl.privateKey != null) {
            certfile = domain.ssl.privateKey.path;
          };
        };
      in c2s ++ s2s ++ cowboy) config.headcounter.vhosts) /* ++ [
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

        configure.enable = true;

        irc.enable = true;
        irc.options = {
          access.atom = "public";
          host = "irc.headcounter.org";
        };

        stats.enable = true;
        stats.options.access.atom = "admin";

        shared_roster.enable = true;

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
            "https://headcounter.org/"
          ];
        };

        roster.enable = true;
        roster.options = {
          access.atom = "public";
          versioning = true;
          store_current_id = false;
        };

        pubsub.enable = true;
        pubsub.options = {
          access_create.atom = "pubsub_createnode";
          max_items_node = 100;
          plugins = map (plugin: { binary = plugin; }) [
            "flat" "hometree" "pep"
          ];
        };

        privacy.enable = true;
        privacy.options.access.atom = "public";

        admin_extra.enable = true;

        caps.enable = true;

        disco.enable = true;
        disco.options = {
          access.atom = "public";
          server_info = let
            mkInfo = { modules ? { atom = "all"; }, field, value }: {
              tuple = [
                (if isList modules then map (m: { atom = m; }) modules
                                   else modules)
                { binary = field; }
                (singleton { binary = value; })
              ];
            };
          in [
            (mkInfo {
              field = "abuse-addresses";
              value = "mailto:abuse@headcounter.org";
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
          extra_domains = map (base: { binary = "${base}.headcounter.org"; }) [
            # TODO: Generate this based on available services!
            "conference"
          ];
        };

        vcard.enable = true;
        vcard.options = {
          access.atom = "public";
          search = false;
          host = "vjud.headcounter.org";
        };

        offline.options = {
          access.atom = "public";
          access_max_user_messages.atom = "max_user_offline_messages";
        };

        private.enable = true;
        private.options.access.atom = "public";

        bosh.enable = true;

        muc.enable = true;
        muc.options = {
          access.atom = "muc";
          access_create.atom = "muc";
          access_persistent.atom = "muc";
          access_admin.atom = "muc_admin";
          host = "conference.headcounter.org";
        };

        ping.enable = true;
        ping.options = {
          send_pings = true;
          ping_interval = 240;
        };

        last.enable = true;
        last.options.access.atom = "public";
      };

      shapers = {
        slow      = 500;
        normal    = 5000;
        fast      = 50000;
        ultrafast = 500000;
      };

      acl.patterns = {
        local.user.regex = "";
        anonymous.server = "anonymous.headcounter.org";
        # Don't allow names that are too short.
        weirdnames.user.regex = "^..?$";
        # Placeholder to quickly block spammy JIDs at runtime.
        blocked = {};
      } // hclib.getcred ["xmpp" "adminACLs"] {
        admin = { user = "admin"; server = "headcounter.org"; };
        wallops = { user = "wallop"; server = "headcounter.org"; };
        torservers_admin = { user = "toradmin"; server = "torservers.net"; };
      };

      acl.rules.access = {
        pubsub_createnode = [ { allow = true; } ];
        configure = [ { allow = true; match = "admin"; } ];
        register = [
          { allow = false; match = "weirdnames"; }
          { allow = false; }
        ];
        public = [ { allow = true; } ];
        announce = [
          { allow = true; match = "admin"; }
          { allow = true; match = "wallops"; }
        ];
        c2s = [
          { allow = false; match = "blocked"; }
          { allow = false; match = "anonymous"; }
          { allow = true; }
        ];
        pollers = [
          { allow = false; match = "blocked"; }
          { allow = true; }
        ];
        muc_admin = [
          { allow = true; match = "admin"; }
        ];
        muc_torservers_admin = [
          { allow = true; match = "torservers_admin"; }
        ];
        muc_wallops = [
          { allow = true; match = "admin"; }
          { allow = true; match = "wallops"; }
        ];
        muc = [ { allow = true; } ];
        local = [ { allow = true; match = "local"; } ];
      };

      acl.rules.limit = {
        max_user_sessions = [ { limit = 10; } ];
        max_user_offline_messages = [
          { limit = 5000; match = "admin"; }
          { limit = 200; }
        ];
      };

      acl.rules.shaper = {
        c2s_shaper = [
          { shaper = null; match = "admin"; }
          { shaper = null; match = "wallops"; }
          { shaper = null; match = "torservers_admin"; }
          { shaper = "normal"; }
        ];
        ft_shaper = [
          { shaper = null; match = "admin"; }
          { shaper = null; match = "wallops"; }
          { shaper = null; match = "torservers_admin"; }
          { shaper = "ultrafast"; }
        ];
        s2s_shaper = [ { shaper = "fast"; } ];
      };

      extraConfig = {
        watchdog_admins = [];

        # See issue #13!
        # {host_config, "anonymous.headcounter.org", [
        #   {auth_method, anonymous},
        #   {allow_multiple_connections, true},
        #   {anonymous_protocol, both}
        # ]}.

        # Default language for server messages
        language = "en";

        s2s_ciphers = serverCiphers;

        domain_certfile.multi = mapAttrsToList (name: domain: {
          extuple = [ domain.fqdn "${domain.ssl.privateKey.path}" ];
        }) (filterAttrs (_: d: d.fqdn != null) config.headcounter.vhosts);
      };
    };
  } // optionalAttrs hclib.hasCredentials {
    cookie = hclib.getcred ["xmpp" "cookie"] (throw "XMPP cookie not found");
  };
}
