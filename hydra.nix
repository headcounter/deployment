{ pkgs, config, ... }:

let
  hydraRelease = import ./hydra/release.nix {};
  hydra = builtins.getAttr config.nixpkgs.system hydraRelease.build;
in {
  require = [ ./hydra/hydra-module.nix ];
  services.hydra = {
    inherit hydra;
    enable = true;
    hydraURL = "http://hydra.headcounter.org/";
    notificationSender = "hydra@headcounter.org";
    dbi = "dbi:Pg:dbname=hydra;";
  };

  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql92;
  services.postgresql.authentication = ''
    local hydra hydra peer
  '';
}
