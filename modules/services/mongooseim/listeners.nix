{ pkgs, ... }:

{ lib, config, ... }:

with lib;
with pkgs.headcounter.nixErlangTools;

{
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
    addrTerm = parseErlIpAddr config.address;
    addrSpec = singleton (erlInt config.port)
            ++ optional (config.address != null) addrTerm
            ++ optional (config.type != null) (erlAtom config.type);
    addr = if length addrSpec == 1 then head addrSpec
           else "{${concatStringsSep ", " addrSpec}}";
  in "{${addr}, ${erlAtom config.module}, ${config.options}}";
}
