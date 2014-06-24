{ nixpkgs ? <nixpkgs>
, system ? builtins.currentSystem
}:

let
  callTest = fn: args: import fn ({
    inherit (nixpkgs) pkgs;
    inherit system;
  } // args);
in {
  mongooseim = callTest ./mongooseim.nix {};
  headcounter = callTest ./headcounter {};
}
