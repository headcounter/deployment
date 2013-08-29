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

    headcounter.subnets.eth0 = {
      prefix = 29;
      addrs = let
        ip = num: "78.47.32.${builtins.toString num}";
      in { # subnet 78.47.32.128/29
        https       = ip 128;
        jabber      = ip 129;
        headcounter = ip 130;
        aszlig      = ip 131;
        noicq       = ip 132;
        "no-icq"    = ip 133;
        nolabel1    = ip 134;
        nolabel2    = ip 135;
      };
    };

    services.headcounter.lighttpd = {
      enable = true;
      modules.proxy.enable = true;

      virtualHosts = with pkgs.lib; singleton {
        type = "static";
        on = "hydra.headcounter.org";
        configuration = ''
          proxy.balance = "hash"
          proxy.server = ("" => ((
            "host" => "127.0.0.1",
            "port" => 3000
          )))
        '';
      };
    };

    require = [ ./common.nix ./hydra.nix ./lighttpd.nix ./subnets.nix ];
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

    require = [ ./common.nix ./hydra-slave.nix ];
  };
}
