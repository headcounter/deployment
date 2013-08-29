{ pkgs, config, ... }:

with pkgs.lib;

let
  cfg = config.headcounter.subnets;

  mapAttrsToString = f: set: concatStrings (mapAttrsToList f set);

  mkSubnet = device: { prefix, addrs, ... }: let
    mkAddr = ip: shortLabel: let
      cidr = "${ip}/${toString prefix}";
      label = "${device}:${shortLabel}";
      cmd = mode: "ip -4 addr ${mode} '${cidr}' dev '${device}'";
      cmdLabel = mode: (cmd mode) + " label '${label}'";
      cmdReadd = "(${cmd "del"} && ${cmdLabel "add"})";
    in ''
      ${cmdLabel "add"} || ${cmdLabel "change"} || ${cmdReadd} || true
    '';
  in mapAttrsToString mkAddr addrs;
in {
  options.headcounter.subnets = mkOption {
    default = {};
    type = types.attrsOf types.optionSet;
    description = "Additional IPv4 subnets.";
    options = {
      prefix = mkOption {
        type = types.int;
        description = "Subnet mask in bits.";
      };
      addrs = mkOption {
        default = {};
        type = types.attrsOf types.string;
        description = "Mapping of device labels to IPv4 addresses.";
        apply = mapAttrs' (flip nameValuePair);
      };
    };
  };

  config.networking.localCommands = mkFooter (mapAttrsToString mkSubnet cfg);
}
