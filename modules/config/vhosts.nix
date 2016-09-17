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
      default = "eth0";
      type = types.string;
      description = ''
        Network device to assign this virtual host to.
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

  mkNetConfig = name: netcfg: let
    cidr4 = "${netcfg.ipv4}/${toString netcfg.ipv4prefix}";
    cidr6 = "${netcfg.ipv6}/${toString netcfg.ipv6prefix}";

    cmd = type: mode: if type == 4 then
      "ip -4 addr ${mode} '${cidr4}' dev '${netcfg.device}'"
    else
      "ip -6 addr ${mode} '${cidr6}' dev '${netcfg.device}'";

    cmdReadd = type: "(${cmd type "del"} && ${cmd type "add"})";

  in optionalString (netcfg.ipv4 != null) ''
    ${cmd 4 "add"} || ${cmd 4 "change"} || ${cmdReadd 4} || true
  '' + optionalString (netcfg.ipv6 != null) ''
    ${cmd 6 "add"} || ${cmd 6 "change"} || ${cmdReadd 6} || true
  '';

  netConfig = concatStrings (mapAttrsToList mkNetConfig cfg.vhosts);

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

  options.headcounter.useSnakeOil = mkOption {
    type = types.bool;
    default = false;
    internal = true;
    description = ''
      Use snakeoil certificates for testing purposes.
    '';
  };

  config = mkIf (cfg.vhosts != {}) ({
    systemd.services = {
      "post-network-setup" = {
        description = "Network virtual host setup";

        after = [ "network-setup.service" ];
        before = [ "network.target" ];
        wantedBy = [ "network.target" ];

        path = [ pkgs.iproute ];

        serviceConfig.Type = "oneshot";
        serviceConfig.RemainAfterExit = true;

        script = netConfig;
      };
    } // optionalAttrs cfg.useSnakeOil {
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
  } // optionalAttrs (options ? deployment) {
    deployment.keys = generatedKeys;
  });
}
