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
    MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAyXbrCIozgXV/pn651+Sy
    W8GT11JhPrZWCKx5N2OF9t/Bqy5G1+j1eqil5hZvKQ45eg7meR+ujknUhOZ2B0LQ
    vecYbR43lpzktMTsxPtjuNLeX2cB7XchW4CXTOAj4HCl3/wwgSBYgem4K7iY/POS
    wWdESLX8LZujzJK4U8UaIFvVd7uz04yv9j8zWLcCqx0yUtSuXnofipkelD0W6P4k
    smmd8af+Sd33hhEx0qqxqVD7X4OdgsrKEZdZkcqaPxaDzGgpAsO6S8zRjqFwhgK8
    6+EfLYdDbXlrJBd06CHxbC1baY515J2ZgVOJD1RoDfIX8hS9gu7/2bU/j+/ynJIX
    8QIDAQAB
    -----END PUBLIC KEY-----
  '';

in {
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
}
