{ pkgs, hclib, toplevelConfig, ... }:

{ config, lib, ... }:

with lib;

let
  inherit (hclib) erlAtom erlString erlInt erlList erlTuple parseErlIpAddr;
in {
  options = {
    overrides = mkOption {
      type = types.listOf types.str;
      default = [ "global" "local" "acls" ];
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
              apply = erlInt;
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

    sessionBackend = mkOption {
      type = types.str;
      # XXX: erlexpr!
      default = "{mnesia, []}";
      example = ''
        {redis, [
          {pool_size, 3},
          {worker_config, [{host, "localhost"}, {port, 6379}]}
        ]}
      '';
      description = ''
        Erlang term specifying the preferred session backend.
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
      type = types.nullOr types.lines;
      default = null;
      example = ''
        {ldap_servers, ["localhost"]}.
        {ldap_encrypt, tls}.
        {ldap_port, 636}.
        {ldap_rootdn, "dc=example,dc=com"}.
        {ldap_password, "******"}.
        {ldap_base, "dc=example,dc=com"}.
        {ldap_uids, [{"mail", "%u@mail.example.org"}]}.
        {ldap_filter, "(objectClass=shadowAccount)"}.
      '';
      description = "Extra lines to append to configuration file.";
    };

    generatedConfigFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      internal = true;
      description = "Generated configuration file";
    };
  };

  config.generatedConfigFile = let
    s2sOptions = with config.s2s; ''
      {s2s_use_starttls, ${erlAtom useStartTLS}}.
      ${optionalString (certfile != null) ''
      {s2s_certfile, ${erlString certfile}}.
      ''}
      {s2s_default_policy, ${erlAtom filterDefaultPolicy}}.
      ${concatStrings (mapAttrsToList (host: allow: ''
      {{s2s_host, ${erlString host}}, ${if allow then "allow" else "deny"}}.
      '') filterHosts)}
      {outgoing_s2s_port, ${erlInt outgoing.port}}.
      {outgoing_s2s_options, [${
        concatStringsSep ", " (map erlAtom outgoing.addressFamilies)
      }], ${toString outgoing.connectTimeout}}.
      ${concatStrings (mapAttrsToList (host: static: ''
        {{s2s_addr, ${erlString host}}, {${static.ipAddress}, ${static.port}}}.
      '') outgoing.staticHosts)}
    '';

  in pkgs.writeText "ejabberd.cfg" ''
    % generic options
    ${flip concatMapStrings config.overrides (what: ''
      ${erlAtom ("override_" + what)}.
    '')}
    {loglevel, ${erlInt config.loglevel}}.

    % virtual hosting
    {hosts, ${erlList config.hosts}}.
    ${optionalString (config.routeSubdomains != null) ''
    {route_subdomains, ${erlAtom config.routeSubdomains}}.
    ''}

    % listeners
    {listen, [
      ${concatStringsSep ",\n  " (map
        (getAttr "generatedConfig") config.listeners
      )}
    ]}.

    % S2S options
    ${s2sOptions}

    % session backend
    {sm_backend, ${config.sessionBackend}}.

    % authentication
    {auth_method, ${erlAtom config.auth.method}}.
    {auth_opts, ${config.auth.options}}.

    ${optionalString (config.odbc.type != null) ''
      % ODBC configuration for ${config.odbc.type}
      {odbc_server, ${erlTuple ([
        { atom = config.odbc.type; }
        config.odbc.host
      ] ++ (optional (config.odbc.port != null) config.odbc.port) ++ [
        config.odbc.database
        config.odbc.username
        config.odbc.password
      ])}}.
      {odbc_pool_size, ${erlInt config.odbc.poolSize}}.
    ''}

    % modules
    {modules, [
      ${config.modules.generatedConfig}
    ]}.

    ${optionalString (config.extraConfig != null) ''
    % extra settings
    ${config.extraConfig}
    ''}
  '';
}
