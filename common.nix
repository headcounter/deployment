{ pkgs, config, ... }:
{
  deployment.targetEnv = "hetzner";

  environment.nix = pkgs.nixUnstable;
  environment.systemPackages = with pkgs; [
    htop
  ];

  nix = {
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
