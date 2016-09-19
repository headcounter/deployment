{ pkgs ? import ../pkgs {}
, system ? builtins.currentSystem
}:

let
  callTest = fn: args: import fn ({
    inherit (pkgs) lib;
    inherit pkgs system;
  } // args);
in {
  code-reload = callTest ./code-reload {};
  dyndns = callTest ./dyndns.nix {};
  mongooseim = callTest ./mongooseim.nix {};
  headcounter = callTest ./headcounter {};
  hclib = pkgs.callPackage ./hclib.nix {};
}
