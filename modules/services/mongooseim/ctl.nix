{ pkgs, lib, config, ... }:

with lib;
with import ./erlexpr.nix;

let
  cfg = config.programs.headcounter.mongooseimctl;
  inherit (config.services.headcounter.mongooseim) package;

  ctlArgsFile = pkgs.writeText "ctl.args" ''
    -setcookie ${shErlEsc erlAtom cfg.cookie}
    -noinput
    -hidden
    -pa ${shErlEsc id "${package.mainAppDir}/ebin"}
    -s ejabberd_ctl
  '';

  mongooseimctl = pkgs.writeScriptBin "mongooseimctl" ''
    #!${pkgs.stdenv.shell}
    exec ${pkgs.erlang}/bin/erl \
      -args_file "${ctlArgsFile}" \
      -sname "$("${pkgs.utillinux}/bin/uuidgen")@${cfg.ctlHost}" \
      -extra "${cfg.destNodeName}" "$@"
  '';

in {
  options.programs.headcounter.mongooseimctl = {
    enable = mkEnableOption "MongooseIM controller";

    ctlHost = mkOption {
      default = config.networking.hostName;
      type = types.str;
      description = ''
        The host part of the node name to use for spawning the client side of
        the controller. A random node name will be generated at runtime.
      '';
    };

    destNodeName = mkOption {
      default = "mongooseim@${config.networking.hostName}";
      type = types.str;
      description = ''
        Erlang node name of the MongooseIM server to control.
      '';
    };

    cookie = mkOption {
      example = "super_secret_random_sequence";
      type = types.str;
      description = ''
        The magic cookie used for communicating with the MongooseIM Erlang node.
      '';
    };
  };

  config = mkIf cfg.enable {
    environment.systemPackages = singleton mongooseimctl;
  };
}
