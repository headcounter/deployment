{ pkgs, lib, options, config, ... }:

with lib;

let
  cfg = config.headcounter;

  domainSubmodule.options = {
    fqdn = mkOption {
      default = null;
      example = "example.com";
      type = types.nullOr (types.uniq types.string);
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
      default = config.headcounter.vhostDefaultDevice;
      type = types.string;
      description = ''
        Network device to assign this virtual host to. By default it's the
        device set by <option>headcounter.vhostDefaultDevice</option>.
      '';
    };

    ssl = {
      privateKey = mkOption {
        default = null;
        type = types.nullOr types.string;
        description = ''
          The PEM-encoded private key as a string value.
        '';
        apply = key: if (key == null) then null else {
          path = "/run/keys/${getPrivkeyFilename key}";
          value = key;
        };
      };

      publicKey = mkOption {
        default = null;
        type = types.nullOr types.string;
        description = ''
          The X.509 public certificate as a string value.
        '';
        apply = key: if (key == null) then null else {
          path = pkgs.writeText "pubkey.pem";
          value = key;
        };
      };

      intermediateCert = mkOption {
        default = null;
        type = types.nullOr types.string;
        description = ''
          Intermediate X.509 certificate chain of the CA as a string value.
        '';
        apply = val: if (val != null)
          then pkgs.writeText "intermediate.pem" val
          else null;
      };
    };
  };

  getPrivkeyFilename = key: "ssl-${builtins.hashString "sha256" key}.key";

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

  generatedKeys = let
    hasPrivKey = name: attrs: attrs.ssl.privateKey != null;
    getPrivkey = name: attrs: with attrs.ssl; {
      name = getPrivkeyFilename privateKey.value;
      value.text = let
        mkImCert = import (pkgs.runCommand "intermediate.nix" {} ''
          cat > "$out" <<NIX
          '''
          $(cat "${intermediateCert}")
          '''
          NIX
        '');
        imcert = optionalString (intermediateCert != null) mkImCert;
        mkval = attr: optionalString (attr != null) attr.value;
      in mkval publicKey + imcert + privateKey.value;
      # XXX: Add an isXMPP option or something like that.
      value.group = "mongoose";
      value.permissions = "0640";
    };
  in mapAttrs' getPrivkey (filterAttrs hasPrivKey cfg.vhosts);
in {
  options.headcounter.vhosts = mkOption {
    default = {};
    type = types.attrsOf (types.submodule domainSubmodule);
    description = ''
      Domains/virtual host configuration.
    '';
  };

  options.headcounter.vhostDefaultDevice = mkOption {
    default = "eth0";
    type = types.str;
    description = ''
      Default network device to use for all vhosts.
    '';
  };

  options.headcounter.useSnakeOil = mkOption {
    type = types.bool;
    default = false;
    internal = true;
    description = ''
      Use snakeoil certificates for testing purposes.
    '';
  };

  config = mkIf (cfg.vhosts != {}) (mkMerge [
    { networking.interfaces = netConfig; }
    (mkIf cfg.useSnakeOil {
      systemd.services = {
        inject-keys = {
          description = "Inject Snakeoil Keys";
          wantedBy = [ "keys.target" ];
          before = [ "keys.target" ];
          unitConfig.DefaultDependencies = false;
          serviceConfig.Type = "oneshot";
          serviceConfig.RemainAfterExit = true;
          script = ''
            mkdir -p /run/keys -m 0750
            chown root:keys /run/keys
          '' + concatStrings (mapAttrsToList (name: value: ''
            cp "${pkgs.writeText name value.text}" "/run/keys/${name}"
            chmod 640 "/run/keys/${name}"
            chown root:keys "/run/keys/${name}"
          '') generatedKeys);
        };
      };
    })
    (optionalAttrs (options ? deployment) {
      deployment.keys = generatedKeys;
    })
  ]);
}
