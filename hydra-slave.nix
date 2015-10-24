{ resources, ... }:

let
  nixosSigningKey = ''
    -----BEGIN PUBLIC KEY-----
    MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAvXmUhovC36OJ9a/3oTaV
    2IvS5gvD87fyG64W/iFKlz2nKWi0JDxq58DNZtaIxSL4sVzQ4pO3M+2Zpgs8+sZ4
    x2mekrlk78VAz3qy8ZZRr0ksw51kL/Ub829ajgnG+2sFedmBBhnRTtGtxLikDQ/D
    x4g4XMgaa11lmobmmwv5mN1ygG0v2GC04lwuMfNNgOCaOTaszRRRKFEdrPhKfHRW
    Ns2nflWz990H/3ohfEDdDMYJ8VBsolI78KrjnttfXu2tqxj7lXZL9XJ1R4vPkpcC
    H3JFbERPXgqIFVxbasYWidpfil/sHjVZgyv2l9CznVDet2eDDgI5NgJQjR+S/wHz
    owIDAQAB
    -----END PUBLIC KEY-----
  '';

in {
  deployment.hetzner.partitions = ''
    clearpart --all --initlabel --drives=sda,sdb

    part raid.11 --size=500 --ondisk=sda
    part raid.12 --size=500 --ondisk=sdb

    part swap1 --size=20000 --label=swap1 --fstype=swap --ondisk=sda
    part swap2 --size=20000 --label=swap2 --fstype=swap --ondisk=sdb

    part raid.21 --grow --ondisk=sda
    part raid.22 --grow --ondisk=sdb

    raid /boot --level=1 --device=md0 --fstype=ext2 --label=boot raid.11 raid.12
    raid /     --level=0 --device=md1 --fstype=ext4 --label=root raid.21 raid.22
  '';

  users.extraUsers.hydrabuild = {
    uid = 1000;
    description = "Hydra build user";
    group = "users";
    home = "/home/hydrabuild";
    useDefaultShell = true;
    createHome = true;
    openssh.authorizedKeys.keys = [
      resources.sshKeyPairs."hydra-build".publicKey
    ];
  };

  environment.etc."nix/signing-key.pub".text = nixosSigningKey;

  systemd.services."enable-ksm" = {
    description = "Enable Kernel Same-Page Merging";
    wantedBy = [ "multi-user.target" ];
    after = [ "systemd-udev-settle.service" ];
    script = ''
      if [ -e /sys/kernel/mm/ksm ]; then
        echo 1 > /sys/kernel/mm/ksm/run
      fi
    '';
  };

  nix.gc.automatic = true;
  nix.gc.dates = "0/6:0";
  nix.gc.options = let
    minSpace = 200; # the minimum of free space we want to have available in GB
    avail = "$(df -k -BG --output=avail /nix/store | sed -ne '$s/[^0-9]//gp')";
    maxFreed = "${toString minSpace} - ${avail}";
  in "--max-freed \"$(((${maxFreed}) * 1024 ** 3))\"";
}
