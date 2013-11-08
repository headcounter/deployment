{ pkgs ? import <nixpkgs> {}, config, ... }:

with pkgs.lib;

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

    sessionBackend = mkOption {
      type = types.str;
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
      {s2s_use_starttls, ${useStartTLS}}.
      ${optionalString (certfile != null) ''
      {s2s_certfile, ${certfile}}.
      ''}
      {s2s_default_policy, ${filterDefaultPolicy}}.
      ${concatStrings (mapAttrsToList (host: allow: ''
      {{s2s_host, "${host}"}, ${if allow then "allow" else "deny"}}.
      '') filterHosts)}
      {outgoing_s2s_port, ${toString outgoing.port}}.
      {outgoing_s2s_options, [${
        concatStringsSep ", " outgoing.addressFamilies
      }], ${toString outgoing.connectTimeout}}.
      ${concatStrings (mapAttrsToList (host: static: ''
        {{s2s_addr, "${host}"}, {${static.ipAddress}, ${static.port}}}.
      '') outgoing.staticHosts)}
    '';

  in pkgs.writeText "ejabberd.cfg" ''
    % generic options
    ${flip concatMapStrings config.overrides (what: ''
      override_${what}.
    '')}
    {loglevel, ${toString config.loglevel}}.

    % S2S options
    ${s2sOptions}

    % Session backend
    {sm_backend, ${config.sessionBackend}}.
  '';
}
