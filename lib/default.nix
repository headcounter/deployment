{ lib ? import <nixpkgs/lib> }:

lib.fold (path: acc: lib.recursiveUpdate acc (import path lib)) {} [
  ./erlang ./module-support.nix ./credentials.nix ./types.nix
]
