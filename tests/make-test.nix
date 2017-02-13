testexpr:

{ system ? builtins.currentSystem, ... }@args: let

  inherit (import <nixpkgs/nixos/lib/testing.nix> {
    inherit system;
  }) makeTest pkgs;

  inherit (pkgs.lib) optionalAttrs mapAttrs;

  testAttrs = if builtins.isFunction testexpr then testexpr (args // {
    pkgs = import ../pkgs { inherit pkgs; };
  }) else testexpr;

  nodes = testAttrs.nodes or (optionalAttrs (testAttrs ? machine) {
    inherit (testAttrs) machine;
  });

  argsWithCommon = removeAttrs testAttrs [ "machine" ] // {
    nodes = mapAttrs (name: config: {
      imports = [ ../common.nix config ];
    }) nodes;
  };

in makeTest argsWithCommon
