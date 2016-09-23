let
  mkMachine = attrs: {
    imports = [ ./common-machines.nix ]
           ++ attrs.imports or [];
  } // removeAttrs attrs [ "imports" ];
in {
  network.description = "Headcounter Services";
  network.enableRollback = true;

  resources.sshKeyPairs."hydra-build" = {};

  ultron = { pkgs, lib, config, ... }: mkMachine {
    imports = [ ./machines/ultron ];
    deployment.hetzner.mainIPv4 = "5.9.105.142";
    deployment.encryptedLinksTo = [ "dugee" "gussh" ];

    services.openssh.extraConfig = lib.mkAfter ''
      ListenAddress ${config.deployment.hetzner.mainIPv4}
      ListenAddress [2a01:4f8:162:4187::]
    '';
  };

  taalo = { pkgs, lib, config, ... }: mkMachine {
    imports = [ ./hydra.nix ];
    deployment.hetzner.mainIPv4 = "188.40.96.202";

    fileSystems."/".options = [
      "autodefrag" "space_cache" "compress=lzo" "noatime"
    ];

    deployment.hetzner.partitions = ''
      clearpart --all --initlabel --drives=sda,sdb

      part swap1 --size=10000 --label=swap1 --fstype=swap --ondisk=sda
      part swap2 --size=10000 --label=swap2 --fstype=swap --ondisk=sdb

      part btrfs.1 --grow --ondisk=sda
      part btrfs.2 --grow --ondisk=sdb

      btrfs / --data=1 --metadata=1 --label=root btrfs.1 btrfs.2
    '';
    deployment.encryptedLinksTo = [ "ultron" ];

    services.hydra.listenHost = lib.mkForce
      config.networking.p2pTunnels.ssh.ultron.localIPv4;
    services.hydra.dbi = let
      inherit (config.networking.p2pTunnels.ssh) ultron;
    in "dbi:Pg:dbname=hydra;user=hydra;host=${ultron.remoteIPv4}";
  };

  benteflork = mkMachine {
    imports = [ ./hydra-slave.nix ];
    deployment.hetzner.mainIPv4 = "144.76.202.147";
  };

  dugee = { lib, ... }: mkMachine {
    imports = [ ./dns-server.nix ];
    deployment.hetzner.mainIPv4 = "78.46.182.124";
    networking.localCommands = lib.mkAfter ''
      ip -6 addr add 2a01:4f8:d13:3009::2 dev eth0
    '';
  };

  gussh = { lib, ... }: mkMachine {
    imports = [ ./dns-server.nix ];
    deployment.hetzner.mainIPv4 = "78.47.142.38";
    networking.localCommands = lib.mkAfter ''
      ip -6 addr add 2a01:4f8:d13:5308::2 dev eth0
    '';
  };

  unzervalt = { nodes, lib, ... }: mkMachine {
    deployment.targetEnv = "container";
    deployment.container.host = nodes.ultron.config;
    imports = [ ./common.nix ]
           ++ lib.optional (lib.pathExists ./private/default.nix) ./private;
    headcounter.services.webspace.enable = true;
    users.mutableUsers = false;
  };
}
