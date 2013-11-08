{ pkgs ? import <nixpkgs> {}, config, ... }:

with pkgs.lib;
with import ./erlexpr.nix;

let
  # TODO: Maybe add a specific type for this, too?
  enumDoc = attrs: ''
    <variablelist>
      ${concatStrings (flip mapAttrsToList attrs (option: doc: ''
        <varlistentry>
          <term><option>${option}</option></term>
          <listitem><para>${doc}</para></listitem>
        </varlistentry>
      ''))}
    </variablelist>
  '';
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
      '' + enumDoc {
        global = "Override global options (shared by all nodes in a cluster).";
        local = "Override local options (specific to this particular node).";
        acls = "Remove the Access Control Lists before new ones are added.";
      };
    };

    loglevel = mkOption {
      type = types.int;
      default = 3;
      description = "Verbosity of logging:" + enumDoc {
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
        type = types.str;
        default = "optional";
        description = ''
          Whether to enable/enforce STARTTLS + Dialback for S2S connections.

          Allowed values are:
        '' + enumDoc {
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
            "example-host1.net".ipAddress = "{1,2,3,4}";
            "example-host2.net".ipAddress = "{5,6,7,8}";
            "example-host2.net".port = 666;
          };
          description = ''
            IP addresses predefined for specific hosts to skip DNS lookups.
            Ports defined here take precedence over <option>port</option>.
          '';

          options = {
            ipAddress = mkOption {
              type = types.str;
              # XXX: Split v4/v6 and transform accordingly into an Erlang term.
              example = "{127,0,0,1}";
              description = ''
                An erlang expression specifying the IPv4 or IPv6 address to use
                for connecting to the host.
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
      type = types.listOf (types.submodule ./listeners.nix);
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

    generatedConfigFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      internal = true;
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
        {{s2s_addr, ${erlString host}}, {${static.ipAddress}, ${erlInt
          static.port}}}.
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

    % Session backend
    {sm_backend, ${config.sessionBackend}}.
  '';
}
