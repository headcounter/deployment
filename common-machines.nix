{ pkgs, config, ... }:
{
  imports = [ ./common.nix ];

  deployment.targetEnv = "hetzner";

  environment.systemPackages = with pkgs; [
    htop iotop
  ];

  services.openntpd.enable = true;

  nix = {
    package = pkgs.nixUnstable;
    nrBuildUsers = 100;
    useChroot = true;
    readOnlyStore = true;
    extraOptions = ''
      build-cores = 0
    '';
  };

  nixpkgs.config.allowUnfree = true;
  hardware.cpu.intel.updateMicrocode = true;

  time.timeZone = "Europe/Berlin";
}
