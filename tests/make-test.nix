testexpr:

{ system ? builtins.currentSystem
, pkgs ? import ../pkgs {}
, lib ? pkgs.lib
, ...
} @ args:

(import <nixpkgs/nixos/lib/testing.nix> {
  inherit system;
}).makeTest (if builtins.isFunction testexpr then testexpr args else testexpr)
