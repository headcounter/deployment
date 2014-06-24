{ pkgs, config, ... }:
{
  deployment.targetEnv = "hetzner";

  networking.firewall.enable = false;

  environment.systemPackages = with pkgs; [
    htop iotop
  ];

  nix = {
    package = pkgs.nixUnstable;
    nrBuildUsers = 100;
    useChroot = true;
    readOnlyStore = true;
    extraOptions = ''
      build-cores = 0
    '';
  };

  hardware.cpu.intel.updateMicrocode = true;

  time.timeZone = "Europe/Berlin";
}
