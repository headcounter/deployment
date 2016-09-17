{
  imports = import ./modules/module-list.nix;
  networking.firewall.enable = false;

  services.journald.extraConfig = ''
    MaxRetentionSec=3month
  '';

  nixpkgs.config.packageOverrides = pkgs: {
    inherit (import ./pkgs { inherit pkgs; }) headcounter;
  };
}
