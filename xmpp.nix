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

  mkCiphers = clist: clist ++ [ "@STRENGTH" ];

  clientCiphers = mkCiphers (aes128fs ++ aes256fs ++ tlsv1 ++ legacy);
  serverCiphers = mkCiphers aes256fs;

  isXmppVHost = attrs: attrs.isXMPP && attrs.fqdn != null;
  xmppVHosts = filterAttrs (const isXmppVHost) config.headcounter.vhosts;

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

  modules = {
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
      host = "pubsub.headcounter.org";
    };

    privacy.enable = true;
    privacy.options.access.atom = "public";

    admin_extra.enable = true;

    caps.enable = true;

    disco.enable = true;
    disco.options = {
      access.atom = "public";
      inherit server_info;
      extra_domains = map (base: { binary = "${base}.headcounter.org"; }) [
        # TODO: Generate this based on available services!
        "conference" "pubsub"
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

    mam_meta.enable = true;
    mam_meta.options = {
      backend.atom = "odbc";
      host = "conference.headcounter.org";
      user_prefs_store.atom = "mnesia";
      odbc_message_format.atom = "internal";
      pm_archive_mode.atom = "never";
      pm = {};
      muc = {};
    };

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

in {
  headcounter.services.acme.domains = mapAttrs' (const (vhost: {
    name = head vhost.ssl.domains;
    value.users = [ "mongoose" ];
    value.reloads = [ "mongooseim" ];
  })) xmppVHosts;

  headcounter.postgresql.databases.mongooseim = {
    users.mongooseim = {
      password = hclib.getcred [ "xmpp" "dbpasswd" ] "verysecure";
    };
    schemaFile = "${pkgs.headcounter.mongooseim.mainAppDir}/priv/pg.sql";
    neededBy = [ "mongooseim.service" ];
  };

  headcounter.services.mongooseim = {
    enable = true;

    hostSettings."torservers.net" = {
      modules = modules // {
        disco.enable = true;
        disco.options = {
          access.atom = "public";
          inherit server_info;
          extra_domains = [
            { binary = "conference.torservers.net"; }
            { binary = "pubsub.headcounter.org"; }
          ];
        };

        muc.enable = true;
        muc.options = {
          access.atom = "muc_torservers";
          access_create.atom = "muc_torservers";
          access_persistent.atom = "muc_torservers";
          access_admin.atom = "muc_torservers_admin";
          host = "conference.torservers.net";
        };

        mam_meta.enable = true;
        mam_meta.options = {
          backend.atom = "odbc";
          host = "conference.torservers.net";
          user_prefs_store.atom = "mnesia";
          odbc_message_format.atom = "internal";
          pm_archive_mode.atom = "never";
          pm = {};
          muc = {};
        };
      };
    };

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
      auth.mechanisms = [ "plain" "digest" "scram" ];

      s2s.filterDefaultPolicy = "allow";
      s2s.useStartTLS = "required";
      s2s.ciphers = serverCiphers;
      s2s.outgoing.port = 5269;
      s2s.outgoing.addressFamilies = [ "ipv6" "ipv4" ];

      s2s.domainCerts = mapAttrs' (name: domain: {
        name = domain.fqdn;
        value = domain.ssl.allInOne;
      }) xmppVHosts;

      listeners = flatten (mapAttrsToList (name: domain: let
        mkAddr = module: attrs: [
          (attrs // { inherit module; address = domain.ipv4; })
          (attrs // { inherit module; address = domain.ipv6; })
        ];

        mkC2S = isLegacy: mkAddr "ejabberd_c2s" ({
          port = if isLegacy then 5223 else 5222;
          options = {
            access.atom = "c2s";
            shaper = "c2s_shaper";
            ciphers = concatStringsSep ":" clientCiphers;
            certfile = domain.ssl.allInOne;
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
            shaper = "s2s_shaper";
            ciphers = concatStringsSep ":" serverCiphers;
            protocol_options = [ "no_sslv2" "no_sslv3" ];
            certfile = domain.ssl.allInOne;
          };
        };
      in c2s ++ s2s ++ cowboy) xmppVHosts);

      odbc = {
        type = "pgsql";
        password = hclib.getcred [ "xmpp" "dbpasswd" ] "verysecure";
      };

      inherit modules;

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
        muc_torservers = [ { allow = true; } ];
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

      # See issue #13!
      # {host_config, "anonymous.headcounter.org", [
      #   {auth_method, anonymous},
      #   {allow_multiple_connections, true},
      #   {anonymous_protocol, both}
      # ]}.
    };
  } // optionalAttrs hclib.hasCredentials {
    cookie = hclib.getcred ["xmpp" "cookie"] (throw "XMPP cookie not found");
  };
}
