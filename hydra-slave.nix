{ resources, ... }:

let
  nixosSSHKey = "ssh-dss "
    + "AAAAB3NzaC1kc3MAAACBAMHRjGSDaBp4Z30JF4S9ApabBCpdr57Ad0aD9oH2A"
    + "/WEFnWYQSAzK4E/HHD2DV2XP1stNkZ1ks2v3F4Yu/veR+qVlUWbJW1RIIfuQg"
    + "kG44K0R3C2qx4BAZUVYzju1NVCJbBOO6ipVY9cfmpokV52HZFhP/2HocTNLoa"
    + "v3F0AsbbJAAAAFQDaJiQdpJBEa4Wr5FfVl1kYqmQZJwAAAIEAwbern5XL+SNI"
    + "Ma+sJ3CBhrWyYExYWiUbdmhQEfyEAUmoPsEr1qpb+0WREic9Nrxz48QWZDK5x"
    + "MvzZyQEkuAMJUBWcdm12rME7WMvg7OZGr9DADjAtfMfj3Ui2XvOuQ3ia/OTsM"
    + "GkQTDWnkOM9Ni128SNSl9urFBlXATdGvo+468AAACBAK8s6LddhhkRqsF/l/L"
    + "2ooS8c8A1rTFWAOy3/sgXFNvMyS/Mig2p966xRrRHr7Bc+H2SuKEE5WmLCXqy"
    + "mgxLHhrFU4zm/W/ej1yB1CAThd4xUfgJu4touJROjvcD1zzlmLeat0fp2k5mC"
    + "uiLKcTKi0vxKWiiopF9nvBBK+7ODPC7 buildfarm@nixos";

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
  deployment.alwaysActivate = false;

  deployment.hetzner.partitions = ''
    clearpart --all --initlabel --drives=sda,sdb

    part swap1 --size=20000 --label=swap1 --fstype=swap --ondisk=sda
    part swap2 --size=20000 --label=swap2 --fstype=swap --ondisk=sdb

    part raid.1 --grow --ondisk=sda
    part raid.2 --grow --ondisk=sdb

    raid / --level=1 --device=md0 --fstype=ext4 --label=root raid.1 raid.2
  '';

  users.extraUsers.hydrabuild = {
    description = "Hydra build user";
    group = "users";
    home = "/home/hydrabuild";
    useDefaultShell = true;
    createHome = true;
    isSystemUser = false;
    openssh.authorizedKeys.keys = [
      resources.sshKeyPairs."hydra-build".publicKey
      nixosSSHKey
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
