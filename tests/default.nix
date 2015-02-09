{ pkgs ? import ../pkgs {}
, system ? builtins.currentSystem
}:

let
  callTest = fn: args: import fn ({
    inherit (pkgs) lib;
    inherit pkgs system;
  } // args);
in {
  mongooseim = callTest ./mongooseim.nix {};
  headcounter = callTest ./headcounter {};
}
