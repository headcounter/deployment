{ config, lib, pkgs, ... }:

{
  imports = import ./modules/module-list.nix;

  options = {
    boot.kernelPackages = let
      tunPatch = {
        name = "tun-poll-fix";
        patch = pkgs.fetchurl {
          url = "https://redmoonstudios.org/~aszlig/fdbc/tun2.patch";
          sha256 = "0axaacmwfy4w9m0gsz8855a98li3akidxgxh5zc4kjr4b5m2al5m";
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
