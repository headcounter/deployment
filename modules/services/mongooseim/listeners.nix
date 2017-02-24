{ pkgs, hclib, ... }:

{ lib, config, ... }:

let
  inherit (lib) mkOption types;

  cowboyDoc = "https://ninenines.eu/docs/en/cowboy/1.0/guide";

  httpModule.options = {
    host = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "example.org";
      description = ''
        The virtual host name to serve requests for the associated
        <option>handler</option>.

        If the value is <literal>null</literal> any host is accepted.

        This allows to use the <link
        xlink:href="${cowboyDoc}/routing/#match_syntax">host match syntax of
        the Cowboy web server</link>.
      '';
    };

    path = mkOption {
      type = types.str;
      default = "/";
      example = "/api/rooms/:id/users/[:user]";
      description = let
      in ''
        The path part of the URL to map to the associated
        <option>handler</option>.

        This allows to use the <link
        xlink:href="${cowboyDoc}/routing/#match_syntax">path match syntax of
        the Cowboy web server</link>.
      '';
    };

    handler = mkOption {
      type = types.str;
      example = "mod_bosh";
      description = ''
        The handler module to use for serving requests, which is any Cowboy
        module, not only the ones that come with MongooseIM.

        Handlers that come with the MongooseIM source tree:

        <simplelist type="inline">
        ${lib.concatMapStrings (handler: ''
          <member><literal>${handler}</literal></member>
        '') [
          /* Retrieved using the following command on the source tree:

             grep 'cowboy\|^routes' apps/ejabberd/src/*.erl | sed -r \
               -e '/\/((ejabberd|mod)_cowboy|mongoose_api(_common)?)\.erl:/d' \
               -e '/^[^:]*: *%/d' -e 's/^.*\/([^\/]+)\.erl:.*$/"\1"/' | sort -u
          */
          "mod_bosh"
          "mod_revproxy"
          "mod_websockets"
          "mongoose_api_admin"
          "mongoose_api_client"
          "mongoose_api_metrics"
          "mongoose_api_users"
          "mongoose_client_api"
          "mongoose_client_api_messages"
          "mongoose_client_api_rooms"
          "mongoose_client_api_rooms_messages"
          "mongoose_client_api_rooms_users"
        ]}
        </simplelist>
      '';
    };

    options = mkOption {
      type = hclib.types.erlPropList;
      default = {};
      example.mongoose_client_api_sse.flag = true;
      description = ''
        Additional options to provide to the <option>handler</option>.
      '';
    };
  };

in {
  options = {
    port = mkOption {
      type = types.int;
      description = "The port to listen on.";
    };

    address = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "2001:6f8:900:72a::2";
      description = "The IPv4 or IPv6 address to listen on.";
    };

    type = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "ws";
      description = "The service type to use for this listener";
    };

    module = mkOption {
      type = types.str;
      example = "ejabberd_c2s";
      description = "The Ejabberd module to use for this service.";
    };

    options = mkOption {
      type = hclib.types.erlPropList;
      default = {};
      example = {
        access.atom = "trusted_users";
        shaper_rule = "fast";
      };
      description = "Options for the corresponding <option>module</option>.";
    };

    http = {
      enable = mkOption {
        type = types.bool;
        default = false;
        example = true;
        description = ''
          Whether this listener module should use
          <literal>ejabberd_cowboy</literal> for HTTP(S).
        '';
      };

      modules = mkOption {
        type = types.listOf (types.submodule httpModule);
        default = [];
        example = [
          { host = "localhost";
            path = "/api";
            handler = "mongoose_api_admin";
          }
          { path = "/api";
            handler = "lasse_handler";
            options.mongoose_client_api_sse.flag = true;
          }
        ];
        description = ''
          A list of HTTP(S) modules (using <literal>ejabberd_cowboy</literal>)
          to enable for this listener.
        '';
      };
    };

    expression = lib.mkOption {
      type = lib.types.attrs;
      internal = true;
      description = ''
        The resulting Nix expression that's translated to an Erlang expression
        for the options given by this submodule.
      '';
    };
  };

  config = lib.mkMerge [
    (lib.mkIf config.http.enable {
      module = "ejabberd_cowboy";
      options.modules = map (module: {
        tuple = [
          (if module.host == null then "_" else module.host)
          module.path
          { atom = module.handler; }
          { __raw = module.options; }
        ];
      }) config.http.modules;
    })
    { expression = let
        inherit (hclib) parseErlIpAddr erlInt erlAtom;
        addrTerm = parseErlIpAddr config.address;
        addrSpec = lib.singleton config.port
                ++ lib.optional (config.address != null) { __raw = addrTerm; }
                ++ lib.optional (config.type != null) { atom = config.type; };
        addr = if lib.length addrSpec == 1 then lib.head addrSpec
               else { tuple = addrSpec; };
      in {
        tuple = [ addr { atom = config.module; } { __raw = config.options; } ];
      };
    }
  ];
}
