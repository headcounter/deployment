{
  imports = import ./modules/module-list.nix;
  networking.firewall.enable = false;

  services.journald.extraConfig = ''
    MaxRetentionSec=3month
  '';

  nixpkgs.config.packageOverrides = pkgs: {
    inherit (import ./pkgs { inherit pkgs; }) headcounter;
    # XXX: This is needed for MongooseIM < 1.6.0 for now
    erlang = pkgs.erlangR17;
  };

  nix.extraOptions = ''
    auto-optimise-store = true
  '';
}
