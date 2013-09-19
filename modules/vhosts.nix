{ pkgs, config, ... }:

with pkgs.lib;

let
  domainOptions = {
    fqdn = mkOption {
      example = "example.com";
      type = types.uniq types.string;
      description = ''
        The fully qualified domain name.
      '';
    };

    ipv4 = mkOption {
      default = null;
      type = types.nullOr types.string;
      description = ''
        IPv4 address to use for this domain.
      '';
    };

    ipv4prefix = mkOption {
      default = 29;
      type = types.int;
      description = ''
        IPv4 subnet prefix length in bits.
      '';
    };

    ipv6 = mkOption {
      default = null;
      type = types.nullOr types.string;
      description = ''
        IPv6 address to use for this domain.
      '';
    };

    ipv6prefix = mkOption {
      default = 64;
      type = types.int;
      description = ''
        IPv6 subnet prefix length in bits.
      '';
    };

    device = mkOption {
      default = "eth0";
      type = types.string;
      description = ''
        Network device to assign this virtual host to.
      '';
    };
  };

  mkNetConfig = name: cfg: let
    cidr4 = "${cfg.ipv4}/${toString cfg.ipv4prefix}";
    cidr6 = "${cfg.ipv6}/${toString cfg.ipv6prefix}";

    cmd = type: mode: if type == 4 then
      "ip -4 addr ${mode} '${cidr4}' dev '${cfg.device}'"
    else
      "ip -6 addr ${mode} '${cidr6}' dev '${cfg.device}'";

    cmdReadd = type: "(${cmd type "del"} && ${cmd type "add"})";

  in optionalString (cfg.ipv4 != null) ''
    ${cmd 4 "add"} || ${cmd 4 "change"} || ${cmdReadd 4} || true
  '' + optionalString (cfg.ipv6 != null) ''
    ${cmd 6 "add"} || ${cmd 6 "change"} || ${cmdReadd 6} || true
  '';

  netConfig = concatStrings (mapAttrsToList mkNetConfig config.vhosts);

in {
  options.vhosts = mkOption {
    default = {};
    type = types.attrsOf types.optionSet;
    options = [ domainOptions ];
    description = ''
      Domains/virtual host configuration.
    '';
  };

  config.networking.localCommands = mkFooter netConfig;
}
