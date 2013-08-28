{
  network.description = "Headcounter Services";
  network.enableRollback = true;

  resources.sshKeyPairs."hydra-build" = {};

  ultron = {
    deployment.hetzner.mainIPv4 = "5.9.105.142";
    deployment.hetzner.partitions = ''
      clearpart --all --initlabel --drives=sda,sdb

      part swap1 --recommended --label=swap1 --fstype=swap --ondisk=sda
      part swap2 --recommended --label=swap2 --fstype=swap --ondisk=sdb

      part btrfs.1 --grow --ondisk=sda
      part btrfs.2 --grow --ondisk=sdb

      btrfs / --data=1 --metadata=1 --label=root btrfs.1 btrfs.2
    '';

    require = [ ./common.nix ];
  };

  benteflork = { pkgs, config, ... }: {
    deployment.hetzner.mainIPv4 = "144.76.61.117";
    deployment.hetzner.partitions = ''
      clearpart --all --initlabel --drives=sda,sdb

      part swap1 --size=20000 --label=swap1 --fstype=swap --ondisk=sda
      part swap2 --size=20000 --label=swap2 --fstype=swap --ondisk=sdb

      part raid.1 --grow --ondisk=sda
      part raid.2 --grow --ondisk=sdb

      raid / --level=1 --device=md0 --fstype=ext4 --label=root raid.1 raid.2
    '';

    require = [ ./common.nix ./hydra.nix ./hydra-slave.nix ];
  };
}
