{
  imports = import ./modules/module-list.nix;
  networking.firewall.enable = false;

  nixpkgs.config.packageOverrides = pkgs: {
    inherit (import ./pkgs { inherit pkgs; }) headcounter;
  };
}
