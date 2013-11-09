{ pkgs ? import <nixpkgs> {}, config, ... }:

with pkgs.lib;
with import ./erlexpr.nix;

{
  options = {
    port = mkOption {
      type = types.int;
      description = "The port to listen on.";
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
      type = erlType erlPropList;
      default = {};
      example = {
        access.atom = "trusted_users";
        shaper_rule = "fast";
      };
      description = "Options for the corresponding <option>module</option>.";
    };

    generatedConfig = mkOption {
      type = types.nullOr types.str;
      default = null;
      internal = true;
      description = "Generated configuration values";
    };
  };

  config.generatedConfig = let
    addr = if config.type == null
           then erlInt config.port
           else "{${erlInt config.port}, ${erlAtom config.type}}";
  in "{${addr}, ${erlAtom config.module}, ${config.options}}";
}
