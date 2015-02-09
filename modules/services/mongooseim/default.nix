{ config, pkgs, lib, ... }:

with lib;
with import ./erlexpr.nix;

let
  cfg = config.services.headcounter.mongooseim;
  package = pkgs.headcounter.mongooseim;

  shErlEsc = escaper: str: let
    doubleSlashed = escape ["\\"] (escaper str);
    shQuoted = replaceChars ["'"] [("'\\'" + "'")] doubleSlashed;
  in "'${shQuoted}'";

  commonArgsFile = pkgs.writeText "common.args" ''
    -sname ${shErlEsc erlAtom cfg.nodeName}
    -setcookie ${shErlEsc erlAtom cfg.cookie}
  '';

  progName = "mongooseim";

  serverArgsFile = pkgs.writeText "server.args" ''
    +K true
    +A 5
    +P 10000000
    -args_file ${shErlEsc id commonArgsFile}
    -sasl releases_dir ${shErlEsc erlString "${package}/releases"}
    -mnesia dir ${shErlEsc erlString cfg.databaseDir}
    -boot ${shErlEsc id "${package}/releases/${progName}"}
    -boot_var RELTOOL_EXT_LIB ${shErlEsc id "${package}/lib"}
    -config ${shErlEsc id "${package}/etc/app.config"}
    -env ERL_MAX_PORTS 250000
    -env ERL_FULLSWEEP_AFTER 2
    -sasl sasl_error_logger false
    -embedded
    -noinput
    -smp
  '';
in {
  options.services.headcounter.mongooseim = {
    enable = mkEnableOption "MongooseIM";

    nodeName = mkOption {
      default = "mongooseim@${config.networking.hostName}";
      type = types.str;
      description = "Erlang OTP node name";
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

    configFile = mkOption {
      default = null;
      example = "${package}/etc/ejabberd.cfg";
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
        inherit pkgs;
        toplevelConfig = config;
      });
      description = "Configuration settings.";
    };
  };

  config = mkIf cfg.enable {
    services.headcounter.mongooseim.cookie = mkDefault (let
      randCookie = pkgs.runCommand "erlang-cookie.nix" {} ''
        cat > "$out" <<RAND
        "$(tr -dc A-Za-z0-9 < /dev/urandom | head -c$((80 + $RANDOM % 100)))"
        RAND
      '';
      preferLocalBuild = true;
    in import randCookie);

    users.extraGroups.mongoose = {};
    users.extraUsers.mongoose = {
      description = "MongooseIM user";
      group = "mongoose";
      home = cfg.databaseDir;
      createHome = true;
    };

    systemd.services.mongooseim = rec {
      description = "MongooseIM XMPP Server";
      wantedBy = [ "multi-user.target" ];
      requires = [ "keys.target" ];
      after = [ "network.target" "fs.target" "keys.target" ];

      environment.EMU = "beam";
      environment.ROOTDIR = package;
      environment.PROGNAME = progName;
      environment.EJABBERD_CONFIG_PATH = if cfg.configFile != null
                                         then cfg.configFile
                                         else cfg.settings.generatedConfigFile;

      serviceConfig.Type = "notify";
      serviceConfig.NotifyAccess = "all";
      serviceConfig.User = "mongoose";
      serviceConfig.Group = "mongoose";
      serviceConfig.PrivateTmp = true;
      serviceConfig.PermissionsStartOnly = true;

      serviceConfig.ExecStart = "@${pkgs.erlang}/bin/erl mongooseim"
                              + " -args_file ${serverArgsFile}";
    };
  };
}
