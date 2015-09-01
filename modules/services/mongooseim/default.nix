{ config, pkgs, lib, ... }:

with lib;
with import ./erlexpr.nix;

let
  cfg = config.services.headcounter.mongooseim;

  progName = "mongooseim";
  gcRoot = "/nix/var/nix/gcroots/running-mongooseim";

  cfgFile = if cfg.configFile != null
            then cfg.configFile
            else cfg.settings.generatedConfigFile;

  inherit (config.programs.headcounter.mongooseimctl) ctlTool;

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
    -env ERL_MAX_PORTS 250000
    -env ERL_FULLSWEEP_AFTER 2
    -sasl sasl_error_logger false
    -embedded
    -noinput
    -smp
  '';

  erlCall = code: ''
    "${pkgs.erlang}/bin/erl_call" \
      -sname ${shErlEsc erlAtom cfg.nodeName} \
      -c ${shErlEsc erlAtom cfg.cookie} \
      -a ${shErlEsc id code}
  '';

  codeReloader = pkgs.writeScript "mongooseim-code-reload" ''
    #!${pkgs.stdenv.shell}
    old_release="$(readlink -f "${gcRoot}")"
    new_release="$(readlink -f "${cfg.package}")"

    if [ "$old_release" != "$new_release" ]; then
      # TODO! This obviously is NOT going to work:
      # cd "$new_release"
      # ${pkgs.rebar}/bin/rebar generate-upgrade \
      #   "previous_release=$old_release" \
      #   "target-dir=/tmp/upgrade"
      exit 1
    fi

    echo "Reloading configuration file:" >&2
    ${erlCall "application set_env [ejabberd, config, ${erlString cfgFile}]"}
    "${ctlTool}" reload_local >&2
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
        inherit pkgs;
        toplevelConfig = config;
      });
      description = "Configuration settings.";
    };
  };

  config = mkMerge [
    {
      services.headcounter.mongooseim = {
        package = mkDefault pkgs.headcounter.mongooseim;
      };
    }
    (mkIf cfg.enable {
      services.headcounter.mongooseim = {
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

      programs.headcounter.mongooseimctl = {
        enable = true;
        ctlHost = head (builtins.match "[^@]+@([^@]+)" cfg.nodeName);
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

      services.headcounter.epmd.enable = true;

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
        after = [ "network.target" "fs.target" "keys.target" ];

        reloadIfChanged = true;

        preStart = ''
          ln -sfn "$(readlink -f "${cfg.package}")" "${gcRoot}"
        '';

        environment.EMU = "beam";
        environment.PROGNAME = progName;

        serviceConfig.Type = "notify";
        serviceConfig.NotifyAccess = "all";
        serviceConfig.User = "mongoose";
        serviceConfig.Group = "mongoose";
        serviceConfig.PrivateTmp = true;
        serviceConfig.PermissionsStartOnly = true;

        serviceConfig.ExecStart = "@${pkgs.erlang}/bin/erl ${progName}"
                                + " -args_file ${serverArgsFile}";

        serviceConfig.ExecReload = "@${codeReloader} mongooseim-code-reload";

        serviceConfig.ExecStop = "@${ctlTool} mongooseim-stop stop";
      };
    })
  ];
}
