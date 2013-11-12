{ pkgs, options, config, ... }:

with pkgs.lib;

let
  cfg = config.headcounter;

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
        apply = pkgs.writeText "intermediate.pem";
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
    getPrivkey = name: attrs: {
      name = getPrivkeyFilename attrs.ssl.privateKey.value;
      value = attrs.ssl.publicKey.value + attrs.ssl.privateKey.value;
    };
  in mapAttrs' getPrivkey (filterAttrs hasPrivKey cfg.vhosts);
in {
  options.headcounter.vhosts = mkOption {
    default = {};
    type = types.attrsOf types.optionSet;
    options = [ domainOptions ];
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
    systemd.services."post-network-setup" = {
      description = "Network virtual host setup";

      after = [ "network-setup.service" ];
      before = [ "network.target" ];
      wantedBy = [ "network.target" ];

      path = [ pkgs.iproute ];

      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;

      script = netConfig;
    };
  } // (if (options ? deployment) then {
    deployment.keys = generatedKeys;
  } else mkIf cfg.useSnakeOil {
    systemd.services.inject-keys = {
      description = "Inject Snakeoil Keys";
      wantedBy = [ "keys.target" ];
      before = [ "keys.target" ];
      unitConfig.DefaultDependencies = false;
      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;
      script = ''
        mkdir -p /run/keys -m 0700
      '' + concatStrings (mapAttrsToList (name: value: ''
        cp "${pkgs.writeText name value}" "/run/keys/${name}"
        chmod 600 "/run/keys/${name}"
      '') generatedKeys);
    };
  }));
}
