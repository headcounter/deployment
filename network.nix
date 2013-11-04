{
  network.description = "Headcounter Services";
  network.enableRollback = true;

  resources.sshKeyPairs."hydra-build" = {};

  ultron = { pkgs, config, ... }: {
    deployment.hetzner.mainIPv4 = "5.9.105.142";
    deployment.hetzner.partitions = ''
      clearpart --all --initlabel --drives=sda,sdb

      part swap1 --recommended --label=swap1 --fstype=swap --ondisk=sda
      part swap2 --recommended --label=swap2 --fstype=swap --ondisk=sdb

      part btrfs.1 --grow --ondisk=sda
      part btrfs.2 --grow --ondisk=sdb

      btrfs / --data=1 --metadata=1 --label=root btrfs.1 btrfs.2
    '';

    imports = [ ./common.nix ./machines/ultron.nix ];
  };

  taalo = { pkgs, config, ... }: {
    imports = [ ./common.nix ./hydra-slave.nix ];
    deployment.hetzner.mainIPv4 = "144.76.61.117";
  };
}
