{ config, pkgs, lib, ... }:

with lib;

let
  containerOpts = { name, config, ... }: {
    options = {
      instance = mkOption {
        type = types.addCheck types.str (s: stringLength s <= 11);
        description = ''
          The instance name of the NixOS container.
          By default it's <literal>ic-$name</literal>.
        '';
      };

      index = mkOption {
        type = types.uniq types.int;
        example = 1;
        description = ''
          Index used for the internal IP address the container will have.
        '';
      };

      forwardSSH = mkOption {
        type = types.listOf types.str;
        default = [];
        example = [ "1.2.3.4:2222" "[1234::dead:beef]:2222" ];
        description = ''
          Forward SSH port of the container to the specified list of host
          addresses.
        '';
      };

      hostAddress = mkOption {
        type = types.str;
        example = "4.3.2.1";
        description = ''
          The IP address to assign to this container on the host side.
          By default it's <literal>10.222.$index.1</literal>.
        '';
      };

      localAddress = mkOption {
        type = types.str;
        example = "4.3.2.2";
        description = ''
          The IP address to assign to this container.
          By default it's <literal>10.222.$index.2</literal>.
        '';
      };

      authorizedKeys = mkOption {
        type = types.listOf types.str;
        default = [];
        description = ''
          List of authorized keys to access the <literal>root</literal>
          user of the container.
        '';
      };
    };

    config = {
      instance = mkDefault "ic-${name}";
      hostAddress = mkDefault "10.222.${toString config.index}.1";
      localAddress = mkDefault "10.222.${toString config.index}.2";
    };
  };

in {
  options.headcounter.imperativeContainers = mkOption {
    type = types.attrsOf (types.submodule containerOpts);
    default = {};
    description = ''
      Containers which are maintained by other people.
    '';
  };

  config = let
    ic = config.headcounter.imperativeContainers;
  in {
    containers = mapAttrs' (name: cfg: {
      name = "ic-${name}";
      value = {
        privateNetwork = true;
        autoStart = true;
        inherit (cfg) hostAddress localAddress;
        path = "/nix/var/nix/profiles/system";
      };
    }) ic;

    networking.nat.internalInterfaces =
      mapAttrsToList (name: const "ve-ic-${name}") ic;

    systemd.services = listToAttrs (flatten (attrValues (mapAttrs (name: cfg: [
      {
        name = "imperative-container-${name}-bootstrap";
        value = let
          instance = "ic-${name}";
          profileBaseDir = "/nix/var/nix/profiles/per-container";
          profileDir = "${profileBaseDir}/${instance}";

          initialSystem = (import <nixpkgs/nixos/lib/eval-config.nix> {
            inherit (config.nixpkgs) system;
            modules = singleton {
              boot.isContainer = true;
              networking.hostName = name;
              networking.useDHCP = false;
              networking.firewall.enable = false;
              services.openssh.enable = true;
              services.openssh.startWhenNeeded = false;
              services.openssh.extraConfig = "UseDNS no";
              users.extraUsers.root.openssh.authorizedKeys = {
                keys = cfg.authorizedKeys;
              };
              nix.package = config.nix.package;
              users.motd = ''
                Welcome new user of the headcounter.org container ${name}.

                This instance is currently running with a default config, so
                please either use nixops to deploy your machine or create a
                /etc/nixos/configuration.nix.

                Be sure to at least touch this file when using nixops to deploy
                your system, otherwise the bootstrapping process will reset your
                configuration back to the initial state.

                You can use this template configuration as a starting point:

                { pkgs, ... }:
                {
                  boot.isContainer = true;
                  networking.hostName = "${name}";
                  networking.useDHCP = false;
                  networking.firewall.enable = false;
                  nix.package = pkgs.nixUnstable;
                  services.openssh.enable = true;
                  services.openssh.startWhenNeeded = false;
                  services.openssh.extraConfig = "UseDNS no";
                  users.extraUsers.root.openssh.authorizedKeys = {
                    keys = [
                      ${concatMapStringsSep "\n      " (key: "\"${key}\"")
                          cfg.authorizedKeys}
                    ];
                  };
                }
              '';
            };
          }).config.system.build.toplevel;

        in {
          description = "Bootstrap imperative container ${name}";
          before = [ "container@${instance}.service" ];
          requiredBy = [ "container@${instance}.service" ];
          unitConfig.ConditionPathExists =
            "!/var/lib/containers/${instance}/etc/nixos/configuration.nix";
          serviceConfig.Type = "oneshot";

          script = ''
            [ -e "${profileBaseDir}" ] || mkdir -m 0700 "${profileBaseDir}"
            [ -e "${profileDir}" ] || mkdir -m 0755 "${profileDir}"
            HOME=/homeless-shelter ${config.nix.package}/bin/nix-env \
              -p "${profileDir}/system" \
              --set "${initialSystem}"
          '';
        };
      }

      {
        name = "imperative-container-${name}-ssh";
        value = {
          description = "SSH proxy to imperative container ${name}";
          serviceConfig = {
            ExecStart = "${pkgs.systemd}/lib/systemd/systemd-socket-proxyd"
                      + " ${cfg.localAddress}:22";
            PrivateTmp = true;
          };
        };
      }
    ]) ic)));

    systemd.sockets = mapAttrs' (name: cfg: {
      name = "imperative-container-${name}-ssh";
      value = {
        description = "SSH proxy socket to imperative container ${name}";
        wantedBy = [ "sockets.target" ];
        socketConfig.ListenStream = cfg.forwardSSH;
      };
    }) ic;
  };
}
