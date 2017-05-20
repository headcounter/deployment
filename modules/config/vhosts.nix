{ pkgs, lib, config, ssl, ... }:

with lib;

let
  cfg = config.headcounter;
  inherit (config.headcounter) mainDevice;

  # XXX: Parse the domain names and figure out the *real* root.
  getRootDomain = dcfg: head dcfg.ssl.domains;
  getOtherDomains = dcfg: tail dcfg.ssl.domains;

  domainSubmodule = { config, ... }: {
    options = {
      fqdn = mkOption {
        default = null;
        example = "example.com";
        type = types.nullOr (types.uniq types.str);
        description = ''
          The fully qualified domain name.
        '';
      };

      ipv4 = mkOption {
        default = null;
        type = types.nullOr types.str;
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
        type = types.nullOr types.str;
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
        default = mainDevice;
        type = types.str;
        description = ''
          Network device to assign this virtual host to. By default it's the
          device set by <option>headcounter.mainDevice</option>.
        '';
      };

      isXMPP = mkOption {
        default = false;
        type = types.bool;
        description = ''
          Whether this virtual host is to be used as an XMPP node.
        '';
      };

      ssl = {
        domains = mkOption {
          type = types.listOf types.str;
          default = optional (config.fqdn != null) config.fqdn;
          description = ''
            Domains for which to get SSL certificates for, defaulting to only
            the value specified in <option>fqdn</option> if not null.

            The domains specified here are only valid if they share a common
            root domain.
          '';
        };

        privateKey = mkOption {
          type = types.path;
          description = ''
            The path to the PEM-encoded private key.
          '';
        };

        certificate = mkOption {
          type = types.path;
          description = ''
            Path to the X.509 public certificate file.
          '';
        };

        chain = mkOption {
          type = types.path;
          description = ''
            Path to the X.509 intermediate chain.
          '';
        };

        fullChain = mkOption {
          type = types.path;
          description = ''
            The <option>certificate</option> and <option>chain</option> in one
            file.
          '';
        };

        allInOne = mkOption {
          type = types.path;
          description = ''
            The <option>fullChain</option> and the <option>privateKey</option>
            in one file.
          '';
        };
      };
    };

    config = mkIf (config.ssl.domains != []) {
      ssl.privateKey  = ssl.${getRootDomain config}.privkey;
      ssl.certificate = ssl.${getRootDomain config}.certificate;
      ssl.chain       = ssl.${getRootDomain config}.chain;
      ssl.fullChain   = ssl.${getRootDomain config}.fullchain;
      ssl.allInOne    = ssl.${getRootDomain config}.full;
    };
  };

  mkNetConfig = name: netcfg: {
    ${netcfg.device} = {
      ip4 = singleton {
        address = netcfg.ipv4;
        prefixLength = netcfg.ipv4prefix;
      };
      ip6 = singleton {
        address = netcfg.ipv6;
        prefixLength = netcfg.ipv6prefix;
      };
    };
  };

  netConfig = let
    merge = zipAttrsWith (const zipper);
    zipper = vals: if isList (head vals) then flatten vals else merge vals;
  in merge (mapAttrsToList mkNetConfig cfg.vhosts);

  acmeConfig = let
    mkAcmeConfig = const (attrs: {
      name = getRootDomain attrs;
      value.otherDomains = getOtherDomains attrs;
    });
    filterCfg = filterAttrs (const (attrs: attrs.fqdn != null));
  in mapAttrs' mkAcmeConfig (filterCfg cfg.vhosts);

in {
  options.headcounter.vhosts = mkOption {
    default = {};
    type = types.attrsOf (types.submodule domainSubmodule);
    description = ''
      Domains/virtual host configuration.
    '';
  };

  options.headcounter.internalNetConfig = mkOption {
    type = types.attrs;
    default = {};
    internal = true;
    description = ''
      An attrset of options from <option>networking.interfaces</option> in
      order to be passed along to our network simulation module in
      <filename>../testing/network.nix</filename>.
    '';
  };

  config = mkIf (cfg.vhosts != {}) {
    networking.interfaces = netConfig;
    headcounter.internalNetConfig = netConfig;
    headcounter.services.acme.domains = acmeConfig;
  };
}
