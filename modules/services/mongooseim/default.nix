{ config, pkgs, lib, hclib, ... }:

with lib;

let
  inherit (hclib) shErlEsc erlAtom erlString erlTermList;
  cfg = config.headcounter.services.mongooseim;

  progName = "mongooseim";
  gcRoot = "/nix/var/nix/gcroots/running-mongooseim";

  cfgFile = cfg.configFile;

  mimlib = import ./lib.nix { inherit config hclib lib; };

  inherit (config.headcounter.programs.mongooseimctl) ctlTool;

  nodeHostname = head (builtins.match "[^@]+@([^@]+)" cfg.nodeName);

  serverArgsFile = pkgs.writeText "server.args" ''
    +K true
    +A 5
    +P 10000000
    -sname ${shErlEsc erlAtom cfg.nodeName}
    -setcookie ${shErlEsc erlAtom cfg.cookie}
    -sasl releases_dir ${shErlEsc erlString "${cfg.package}/releases"}
    -mnesia dir ${shErlEsc erlString cfg.databaseDir}
    -boot ${shErlEsc id "${cfg.package}/releases/${progName}"}
    -boot_var RELTOOL_EXT_LIB ${shErlEsc id "${cfg.package}/lib"}
    -config ${shErlEsc id "${cfg.package}/etc/app.config"}
    -ejabberd config ${shErlEsc erlString cfgFile}
    ${mimlib.loopbackArg}
    ${mimlib.inetArg}
    -env ERL_MAX_PORTS 250000
    -env ERL_FULLSWEEP_AFTER 2
    -sasl sasl_error_logger false
    -setup verify_directories false
    -embedded
    -noinput
    -smp
  '';

  codeReloader = pkgs.writeScript "mongooseim-code-reload" ''
    #!${pkgs.stdenv.shell}
    old_release="$(readlink -f "${gcRoot}")"
    new_release="$(readlink -f "${cfg.package}")"

    if [ "$old_release" != "$new_release" ]; then
      # TODO: Wait until MongooseIM 2.1.0 and use relx.
      exec systemctl restart mongooseim
    else
      echo "Reloading configuration file:" >&2
      "${ctlTool}" set_config ${escapeShellArg cfgFile}
      "${ctlTool}" reload_cluster >&2
    fi
  '';

  defaultCfgFile = let
    topLevel = cfg.settings.expression;
    terms = topLevel // mapAttrs' (name: settings: {
      name = "host_config";
      value.extuple = [ name settings.expression ];
    }) cfg.hostSettings;
  in pkgs.writeText "mongooseim.cfg" (erlTermList terms);

in {
  options.headcounter.services.mongooseim = {
    enable = mkEnableOption "MongooseIM";

    nodeName = mkOption {
      default = "mongooseim@${config.networking.hostName}";
      type = types.str;
      description = "Erlang OTP node name";
    };

    nodeIp = mkOption {
      type = types.nullOr types.str;
      default = "127.0.0.1";
      description = ''
        IP address of the node specified in <option>nodeName</option>.

        This is circumventing the <filename>/etc/hosts</filename> file inside
        the Erlang VM. If you don't want this you can set this to
        <literal>null</literal>.
      '';
    };

    cookie = mkOption {
      default = null;
      example = "super_secret_random_sequence";
      type = types.str;
      description = ''
        The magic cookie is used for Erlang nodes to communicate with each
        other, which includes MongooseIM nodes as well as Erlang shells attached
        to a particular node.

        <note><para>
        Using the default will use a non-deterministic randomized value, so it
        might not be desirable to do so if you want to link nodes in a cluster.
        </para></note>
      '';
    };

    package = mkOption {
      type = types.package;
      description = ''
        The MongooseIM package to use for this instance.
      '';
    };

    configFile = mkOption {
      default = null;
      example = "${cfg.package}/etc/ejabberd.cfg";
      type = types.nullOr types.path;
      description = ''
        Path to the main configuration file.
        This overrides all options defined in <option>settings</option>.
      '';
    };

    databaseDir = mkOption {
      default = "/var/db/mongoose";
      type = types.path;
      description = "Database directory for Mnesia";
    };

    settings = mkOption {
      default = {};
      type = types.submodule (import ./settings.nix {
        inherit pkgs hclib;
        toplevelConfig = config;
      });
      description = "Configuration settings.";
    };

    hostSettings = mkOption {
      default = {};
      type = types.attrsOf (types.submodule (import ./settings.nix {
        inherit pkgs hclib;
        toplevelConfig = config;
        hostSpecific = true;
      }));
      description = ''
        Host-specific configuration settings, where the attribute name is a
        virtual host and its value is an attribute set similar to those options
        in <option>settings</option>.
      '';
    };
  };

  config = mkMerge [
    { headcounter.services.mongooseim = {
        package = mkDefault pkgs.headcounter.mongooseim;
      };
    }
    (mkIf (cfg.enable && cfg.nodeIp != null) {
      headcounter.erlang-inet.hosts.${nodeHostname} = cfg.nodeIp;
    })
    (mkIf cfg.enable {
      headcounter.services.mongooseim = {
        configFile = mkDefault defaultCfgFile;
        cookie = mkDefault (let
          randCookie = pkgs.runCommand "erlang-cookie.nix" {
            preferLocalBuild = true;
          } ''
            cat > "$out" <<RAND
            "$(tr -dc A-Za-z0-9 < /dev/urandom | \
               head -c$((80 + $RANDOM % 100)))"
            RAND
          '';
        in import randCookie);
      };

      headcounter.programs.mongooseimctl = {
        enable = true;
        ctlHost = nodeHostname;
        destNodeName = cfg.nodeName;
        inherit (cfg) cookie;
      };

      users.extraGroups.mongoose = {};
      users.extraUsers.mongoose = {
        description = "MongooseIM user";
        group = "mongoose";
        home = cfg.databaseDir;
        createHome = true;
      };

      headcounter.services.epmd.enable = true;

      systemd.services.mongooseim = rec {
        description = "MongooseIM XMPP Server";

        # IMPORTANT: Make sure that you only specify weak dependencies here,
        # because we don't EVER want to stop MongooseIM on any hard dependencies
        # except reboot. Ordering dependencies is fine for first startup, but we
        # really don't care about whether one of the dependencies were stopped
        # or not, we just want to sure that it will NEVER stop except explicitly
        # done so via "systemctl restart mongooseim".
        #
        # If for whatever reason you add a hard dependency nevertheless, make
        # sure to test it properly in the code-reload VM test.
        wantedBy = [ "multi-user.target" ];
        wants = [ "keys.target" ];

        after = let
          dbType = cfg.settings.odbc.type or null;
          dbUnit = if dbType == "pgsql" then "postgresql.service"
                   else if dbType == "mysql" then "mysql.service"
                   else throw "Unsupported ODBC backend type ${dbType}.";
          maybeDbUnit = optional (dbType != null) dbUnit;
        in [ "network.target" "fs.target" "keys.target" ] ++ maybeDbUnit;

        preStart = ''
          ln -sfn "$(readlink -f "${cfg.package}")" "${gcRoot}"
        '';

        reloadIfChanged = true;

        environment.EMU = "beam";
        environment.PROGNAME = progName;

        serviceConfig.Type = "notify";
        serviceConfig.NotifyAccess = "all";
        serviceConfig.User = "mongoose";
        serviceConfig.Group = "mongoose";
        serviceConfig.PrivateTmp = true;
        serviceConfig.PermissionsStartOnly = true;
        serviceConfig.TimeoutStartSec = 0;

        serviceConfig.ExecStart = "@${pkgs.erlang}/bin/erl ${progName}"
                                + " -args_file ${serverArgsFile}";

        serviceConfig.ExecReload = "@${codeReloader} mongooseim-code-reload";

        serviceConfig.ExecStop = "@${ctlTool} mongooseim-stop stop";
      };
    })
  ];
}
