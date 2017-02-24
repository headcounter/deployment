{ pkgs, hclib, toplevelConfig, ... }:

{ config, lib, ... }:

with lib;

let
  inherit (hclib) erlAtom erlString erlInt erlList erlTermList erlTuple
                  parseErlIpAddr;
in {
  options = {
    overrides = mkOption {
      type = types.listOf types.str;
      default = [ "global" "local" "acls" ];
      apply = map (what: "override_${what}");
      description = ''
        Overide options stored in the internal settings database.

        <important>It's not recommended to change these defaults, because you
        will lose determinism in the deployment!</important>

        Valid overrides are:
      '' + hclib.enumDoc {
        global = "Override global options (shared by all nodes in a cluster).";
        local = "Override local options (specific to this particular node).";
        acls = "Remove the Access Control Lists before new ones are added.";
      };
    };

    loglevel = mkOption {
      type = mkOptionType {
        name = "number from 0 to 5";
        check = x: x >= 0 && x <= 5;
        merge = mergeOneOption;
      };
      default = 3;
      description = "Verbosity of logging:" + hclib.enumDoc {
        "0" = "No log at all";
        "1" = "Critical errors";
        "2" = "Normal errors";
        "3" = "Warnings";
        "4" = "Info";
        "5" = "Debug";
      };
    };

    hosts = mkOption {
      type = types.listOf types.str;
      default = singleton "localhost";
      example = [ "example.net" "example.com" "example.org" ];
      description = "List of domains to be served.";
    };

    routeSubdomains = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = let
        url1 = "http://xmpp.org/rfcs/rfc3920.html#rules.subdomain";
        url2 = "http://tools.ietf.org/html/draft-saintandre-rfc3920bis-09"
             + "#section-11.3";
      in ''
        Defines if the server must route stanzas directed to subdomains locally
        (compliant with <link xlink:href="${url1}">RFC 3920 XMPP Core</link>),
        or to foreign server using S2S (compliant with <link
        xlink:href="${url2}">RFC 3920 bis</link>).

        Valid values are <option>local</option> and <option>s2s</option>.
      '';
    };

    s2s = {
      useStartTLS = mkOption {
        type = types.enum [ "false" "optional" "required" "required_trusted" ];
        default = "false";
        description = ''
          Whether to enable/enforce STARTTLS + Dialback for S2S connections.

          Allowed values are:
        '' + hclib.enumDoc {
          "false" = "No STARTTLS at all";
          "optional" = "Only if the endpoint supports it";
          "required" = "Don't allow unencrypted connections";
          "required_trusted" = "Enforce it and validate certificate";
        };
      };

      certfile = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = ''
          Global certificate file for the S2S service.
          This can be overriden on a per-domain basis in XXX.
        '';
      };

      filterHosts = mkOption {
        type = types.attrsOf types.bool;
        default = {};
        example = {
          "goodhost.org" = true;
          "badhost.org" = false;
        };
        description = ''
          Allow or deny communication with specific servers.
        '';
      };

      filterDefaultPolicy = mkOption {
        type = types.str;
        default = "deny";
        description = ''
          Default S2S policy for undefined hosts in
          <option>filterHosts</option>.
        '';
      };

      outgoing = {
        port = mkOption {
          type = types.int;
          default = 5269;
          description = ''
            Default port to use for outgoing connections.
          '';
        };

        addressFamilies = mkOption {
          type = types.listOf types.str;
          default = [ "ipv4" "ipv6" ];
          description = ''
            Preferred address families (which to try first).
          '';
        };

        connectTimeout = mkOption {
          type = types.int;
          default = 10000;
          description = ''
            Connection timeout in milliseconds.
          '';
        };

        staticHosts = mkOption {
          type = types.attrsOf types.optionSet;
          default = {};
          example = {
            "example-host1.net".ipAddress = "1.2.3.4";
            "example-host2.net".ipAddress = "5.6.7.8";
            "example-host2.net".port = 666;
          };
          description = ''
            IP addresses predefined for specific hosts to skip DNS lookups.
            Ports defined here take precedence over <option>port</option>.
          '';

          options = {
            ipAddress = mkOption {
              type = types.str;
              example = "127.0.0.1";
              apply = parseErlIpAddr;
              description = ''
                An IPv4 or IPv6 address to use for connecting to the host.
              '';
            };

            port = mkOption {
              type = types.int;
              default = 5269;
              description = ''
                Port to use for connecting with the host.
              '';
            };
          };
        };
      };
    };

    listeners = mkOption {
      type = types.listOf (types.submodule (import ./listeners.nix {
        inherit pkgs hclib;
      }));
      default = [
        { port = 5280;
          module = "mod_bosh";
          options.num_acceptors = 10;
        }
        { port = 5222;
          module = "ejabberd_c2s";
          options.access.atom = "c2s";
          options.shaper.atom = "c2s_shaper";
          options.max_stanza_size = 65536;
        }
        { port = 5288;
          type = "ws";
          module = "mod_websockets";
          options.host = "localhost";
          options.prefix = "/ws-xmpp";
        }
        { port = 5269;
          module = "ejabberd_s2s_in";
          options.shaper.atom = "s2s_shaper";
          options.max_stanza_size = 131072;
        }
      ];
      description = ''
        Configuration of listeners and their corresponding modules.
      '';
    };

    sessionManagement = {
      backend = mkOption {
        type = types.enum [ "mnesia" "redis" ];
        default = "mnesia";
        example = "redis";
        description = ''
          Session management backend to use.
        '';
      };

      options = mkOption {
        type = hclib.types.erlPropList;
        default = {};
        example.pool_size = 3;
        example.worker_config = {
          host = "localhost";
          port = 6379;
        };
        description = ''
          Options for the session management <option>backend</option>.
        '';
      };
    };

    shapers = mkOption {
      type = types.attrsOf (types.nullOr types.int);
      default = {};
      example = {
        normal = 1000;
        fast = 50000;
      };
      description = ''
        An attribute set of shapers with the shaper name as the key and the
        maximum rate (or <literal>null</literal> for unlimited rate) in bytes
        per second as the value.
      '';
    };

    auth = {
      method = mkOption {
        type = types.enum [ "internal" "external" "odbc" "pam" "ldap" ];
        default = "internal";
        example = "ldap";
        description = ''
          Method used to authenticate the users.

          Valid options are:
        '' + hclib.enumDoc {
          internal = "Against internal Mnesia database";
          external = "Using external script";
          odbc = "Using Open Database Connectivity";
          pam = "Using Pluggable Authentication Modules";
          ldap = "Using Lightweight Directory Access Protocol";
        };
      };

      mechanisms = mkOption {
        type = types.nullOr (types.listOf (types.enum [
          "plain" "digest" "scram" "anonymous" "oauth"
        ]));
        default = null;
        description = ''
          SASL mechanism to use for authentication.

          The default (<literal>null</literal>) will enable all mechanisms.

          Valid options are:
        '' + hclib.enumDoc {
          plain = "Use plaintext passwords";
          digest = "DIGEST-MD5 based auth";
          scran = "SCRAM authentication";
          anonymous = "No authentication using a random user name";
          oauth = "Authentication using an OAuth token";
        };
      };

      options = mkOption {
        type = hclib.types.erlPropList;
        default.password_format.atom = "plain";
        default.scram_iterations = 4096;
        example = {
          password_format.atom = "scram";
          scram_iterations = 32768;
          extauth_program = "/my/custom/auth/script.py";
        };
        description = ''
          Options used for authentication, such as password format and
          security.
        '';
      };
    };

    acl = {
      patterns = mkOption {
        type = types.attrsOf (types.submodule (import ./acl.nix));
        default = {};
        # XXX: Fix this properly after we have switched to NixOS 17.03 because
        # we actually want this to be either a list of ACL submodules or simply
        # an ACL submodule that gets merged with other lists rather than being
        # transformed here.
        apply = mapAttrs (const (val: if isList val then val else [ val ]));
        example.local.user.regex = "";
        example.shortnames.user.regex = "^..$";
        example.alice.user = "alice";
        example.alice.server = "example.org";
        description = ''
          An attribute set of names to pattern options.

          A pattern option is any combination of the <option>user</option>,
          <option>server</option> and <option>resource</option> options or
          alternatively the <option>all</option> option which matches on
          everything and overrides the former suboptions.

          The names are arbitrary names which then can be referenced in
          <option>rules</option>.
        '';
      };

      rules = let
        mkRuleOption = { type, ... }@attrs: mkOption (attrs // {
          type = let
            submodule = types.submodule (import ./access.nix {
              inherit type;
              optPrefix = "headcounter.services.mongooseim.settings";
              patterns = config.acl.patterns;
            });
          in types.attrsOf (types.listOf submodule);
        });
      in {
        shaper = mkRuleOption {
          type = "shaper";
          default = {};
          example.c2s_shaper = [
            { match = "admin"; shaper = "none"; }
            { shaper = "normal"; }
          ];
          description = ''
            Rules for traffic shapers, which can be used in the options of
            various modules.
          '';
        };

        limit = mkRuleOption {
          type = "limit";
          default = {};
          example.max_user_offline_messages = [
            { match = "admin"; limit = 5000; }
            { limit = 100; }
          ];
          description = ''
            Rules for limits (like for example maximum requests) for use in the
            options of some modules.
          '';
        };

        access = mkRuleOption {
          type = "access";
          default = {};
          example.muc_create = [ { match = "local"; allow = true; } ];
          example.muc_admin  = [ { match = "admin"; allow = true; } ];
          description = ''
            Rules for access rights, typically used in the
            <option>access</option> option of various modules.
          '';
        };
      };
    };

    odbc = {
      type = mkOption {
        type = types.nullOr (types.enum [ "mysql" "pgsql" ]);
        default = null;
        description = ''
          Database type to use for all modules configured to use ODBC.
        '';
      };
      host = mkOption {
        type = types.str;
        default = "localhost";
        description = ''
          Host name or IP address to use for connection to the DBMS.
        '';
      };
      port = mkOption {
        type = with types; nullOr (addCheck int (p: p <= 65535 && p >= 0));
        default = null;
        example = 5432;
        description = ''
          Port number to use for connecting to the DBMS or
          <literal>null</literal> for the default port.
        '';
      };
      database = mkOption {
        type = types.str;
        default = "mongooseim";
        description = ''
          Database name to use for connecting to the DBMS.
        '';
      };
      username = mkOption {
        type = types.str;
        default = "mongooseim";
        description = ''
          User name for authenticating against the DBMS.
        '';
      };
      password = mkOption {
        type = types.str;
        example = "verysecurepassword";
        description = ''
          User name for authenticating against the DBMS.
        '';
      };
      poolSize = mkOption {
        type = types.int;
        default = 10;
        description = ''
          How many DB client workers should be started per each domain.
        '';
      };
    };

    modules = mkOption {
      type = types.submodule (import ./modules.nix {
        inherit pkgs hclib toplevelConfig;
        defaults = {
          adhoc.enable = true;
          blocking.enable = true;
          carboncopy.enable = true;
          csi.enable = true;
          disco.enable = true;
          last.enable = true;
          muc.enable = true;
          muc.options.host = "muc.${toplevelConfig.networking.hostName}";
          muc.options.access.atom = "muc";
          muc.options.access_create.atom = "muc_create";
          offline.enable = true;
          privacy.enable = true;
          private.enable = true;
          pubsub.enable = true;
          register.enable = true;
          register.options.welcome_message = ""; # TODO?
          register.options.ip_access = [
            { tuple = [ "allow" "127.0.0.1/8" ]; }
            { tuple = [ "deny" "0.0.0.0/0" ]; }
          ];
          register.options.access.atom = "register";
          roster.enable = true;
          sic.enable = true;
          stream_management.enable = true;
          vcard.enable = true;
          vcard.options.allow_return_all = true;
          vcard.options.search_all_hosts = true;
        };
      });
      default = {};
      description = ''
        Modules enabled for this particular virtual host.
      '';
    };

    extraConfig = mkOption {
      type = types.attrs;
      default = {};
      example = {
        ldap_servers = ["localhost"];
        ldap_encrypt.atom = "tls";
        ldap_port = 636;
        ldap_rootdn = "dc=example,dc=com";
        ldap_password = "foobar";
        ldap_base = "dc=example,dc=com";
        ldap_uids = [ { tuple = ["mail" "%u@mail.example.org"]; } ];
        ldap_filter = "(objectClass=shadowAccount)";
      };
      description = "Extra options to append to configuration file.";
    };

    generatedConfigFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      internal = true;
      description = "Generated configuration file";
    };
  };

  config.generatedConfigFile = pkgs.writeText "mongooseim.cfg" (erlTermList ({
    loglevel = config.loglevel;
    hosts = config.hosts;

    # XXX: Let's remove generatedConfig in the future and pass through Nix
    # types instead of a string.
    listen = map (lcfg: { __raw = lcfg.generatedConfig; }) config.listeners;

    s2s_use_starttls.atom = config.s2s.useStartTLS;
    s2s_default_policy.atom = config.s2s.filterDefaultPolicy;
    outgoing_s2s_port = config.s2s.outgoing.port;
    outgoing_s2s_options.extuple = [
      (map (oaf: { atom = oaf; }) config.s2s.outgoing.addressFamilies)
      config.s2s.outgoing.connectTimeout
    ];

    s2s_host.multi = mapAttrsToList (host: allow: {
      freekey.tuple = [ { atom = "s2s_host"; } host ];
      value.atom = if allow then "allow" else "deny";
    }) config.s2s.filterHosts;

    s2s_addr.multi = mapAttrsToList (host: static: {
      freekey.tuple = [ { atom = "s2s_addr"; } host ];
      value.tuple = [ { __raw = static.ipAddress; } static.port ];
    }) config.s2s.outgoing.staticHosts;

    sm_backend.tuple = [
      { atom = config.sessionManagement.backend; }
      { __raw = config.sessionManagement.options; } # FIXME: Rrrraawwwww!
    ];

    shaper.multi = mapAttrsToList (name: maxrate: let
      val = if maxrate == null then { atom = "none"; }
            else { tuple = [ { atom = "maxrate"; } maxrate ]; };
    in {
      extuple = [ { atom = name; } val ];
    }) config.shapers;

    auth_method.atom = config.auth.method;
    auth_opts.__raw = config.auth.options; # FIXME: Don't use __raw!

    acl.multi = let
      mkMatch = name: pattern: optional (pattern.match != null) {
        extuple = [ { atom = name; } pattern.match ];
      };
      mkPatterns = name: concatMap (mkMatch name);
      patterns = mapAttrsToList mkPatterns config.acl.patterns;
    in concatLists patterns;

    access.multi = let
      genExpr = name: acls: {
        extuple = [
          { atom = name; }
          (map (acl: acl.expression) acls)
        ];
      };
      ruleSets = with config.acl.rules; [ shaper limit access ];
    in concatMap (mapAttrsToList genExpr) ruleSets;

    modules.__raw = "[\n  ${config.modules.generatedConfig}\n]"; # FIXME: Same!
  } // optionalAttrs (config.routeSubdomains != null) {
    route_subdomains.atom = config.routeSubdomains;
  } // optionalAttrs (config.auth.mechanisms != null) {
    sasl_mechanisms = map (mech: {
      atom = "cyrsasl_${mech}";
    }) config.auth.mechanisms;
  } // optionalAttrs (config.odbc.type != null) {
    odbc_server.tuple = [
      { atom = config.odbc.type; }
      config.odbc.host
    ] ++ (optional (config.odbc.port != null) config.odbc.port) ++ [
      config.odbc.database
      config.odbc.username
      config.odbc.password
    ];
    odbc_pool_size = config.odbc.poolSize;
  } // optionalAttrs (config.s2s.certfile != null) {
    s2s_certfile = config.s2s.certfile;
  } // genAttrs config.overrides (const { flag = true; })
    // config.extraConfig));
}
