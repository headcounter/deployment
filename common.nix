{ config, lib, pkgs, ... }:

{
  imports = import ./modules/module-list.nix;

  options = {
    boot.kernelPackages = let
      tunPatch = rec {
        name = "tun-poll-fix";
        patch = pkgs.fetchurl {
          name = "${name}.patch";
          url = "https://patchwork.ozlabs.org/patch/737900/raw/";
          sha256 = "18330picypxbaq7and7zjjv6pkmxp1fhc200dzn6ajamnzyfy2p9";
        };
      };
    in assert config.system.nixosRelease == "16.09"; lib.mkOption {
      apply = kernelPackages: let
        newKernel = kernelPackages.kernel.override {
          kernelPatches = kernelPackages.kernel.kernelPatches
                       ++ lib.singleton tunPatch;
        };
      in pkgs.linuxPackagesFor newKernel config.boot.kernelPackages;
    };
  };

  config = {
    networking.firewall.enable = false;

    services.journald.extraConfig = ''
      MaxRetentionSec=3month
    '';

    nixpkgs.config.packageOverrides = pkgs: {
      inherit (import ./pkgs { inherit pkgs; }) headcounter;
    };
  };
}
