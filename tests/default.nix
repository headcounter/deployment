{ nixpkgs ? <nixpkgs>
, system ? builtins.currentSystem
, minimal ? false
}:

with import "${nixpkgs}/nixos/lib/testing.nix" {
  inherit system minimal;
};

{
  mongooseim = makeTest (import ./mongooseim.nix);
  headcounter = makeTest (import ./headcounter);
}
