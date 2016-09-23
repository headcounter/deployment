{ config, lib, ... }:

with lib;

{
  options.headcounter.nix.sshBuild = {
    enable = mkEnableOption "building using SSH";
    keys = mkOption {
      type = types.listOf types.str;
      default = [];
      example = [ "ssh-dss AAAAB3NzaC1k... alice@example.org" ];
      description = "A list of SSH public keys allowed build via SSH.";
    };
  };

  config = mkIf config.headcounter.nix.sshBuild.enable {
    services.openssh.enable = true;

    services.openssh.extraConfig = ''
      Match User nix-remote-build
        AllowAgentForwarding no
        AllowTcpForwarding no
        PermitTTY no
        PermitTunnel no
        X11Forwarding no
        ForceCommand ${config.nix.package}/bin/nix-store --serve --write
      Match All
    '';

    users.extraUsers.nix-remote-build = {
      description = "Nix SSH build user";
      uid = 412;
      useDefaultShell = true;
      openssh.authorizedKeys.keys = config.headcounter.nix.sshBuild.keys;
    };

    nix.trustedUsers = [ "nix-remote-build" ];
  };
}
