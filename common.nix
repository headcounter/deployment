{
  imports = import ./modules/module-list.nix;
  networking.firewall.enable = false;

  services.journald.extraConfig = ''
    MaxRetentionSec=3month
  '';

  nixpkgs.config.packageOverrides = super: let
    pkgs = super // self;
    self = {
      inherit (import ./pkgs { inherit pkgs; }) headcounter;

      # https://bugs.erlang.org/browse/ERL-340
      erlang = super.erlang.overrideDerivation (drv: {
        patches = (drv.patches or []) ++ pkgs.lib.singleton (pkgs.fetchpatch {
          url = "https://github.com/erlang/otp/commit/"
              + "e27119948fc6ab28bea81019720bddaac5b655a7.patch";
          sha256 = "1605hsdck7ng49yn9mjk7vsk12qr12ypjiwd7810ra9bva8x42m9";
        });
      });
    };
  in self;
}
