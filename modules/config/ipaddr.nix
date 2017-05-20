{ options, config, lib, ... }:

{
  options.headcounter.mainIPv4 = lib.mkOption {
    type = lib.types.nullOr lib.types.str;
    default = null;
    description = ''
      The main IPv4 address of this host.

      This is mainly useful for deployment-wide usage on other nodes, like for
      example for DNS records.

      This in turn also sets <option>deployment.hetzner.mainIPv4</option>.
    '';
  };

  options.headcounter.mainIPv6 = lib.mkOption {
    type = lib.types.nullOr lib.types.str;
    default = null;
    description = ''
      The main IPv6 address of this host.

      This is mainly useful for deployment-wide usage on other nodes, like for
      example for DNS records.
    '';
  };

  options.headcounter.mainDevice = lib.mkOption {
    type = lib.types.str;
    default = "eth0";
    description = ''
      The network device where the <option>headcounter.mainIPv4</option> and
      the <option>headcounter.mainIPv6</option> address reside.
    '';
  };

  config = let
    maybeConfig = lib.optionalAttrs (options ? deployment) {
      deployment.hetzner.mainIPv4 = config.headcounter.mainIPv4;
    };
  in lib.mkIf (config.headcounter.mainIPv4 != null) maybeConfig;
}
