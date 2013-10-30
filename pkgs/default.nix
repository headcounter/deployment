{ pkgs ? import <nixpkgs> {} }:

let
  callPackage = pkgs.lib.callPackageWith (pkgs // self);

  self = rec {
    buildErlang = callPackage ./build-support/build-erlang.nix {};

    mongooseim = callPackage ./mongooseim.nix {};

    # dependencies for mongooseim
    alarms = callPackage ./erldeps/alarms.nix {};
    bear = callPackage ./erldeps/bear.nix {};
    cowboy = callPackage ./erldeps/cowboy.nix {};
    cuesport = callPackage ./erldeps/cuesport.nix {};
    exml = callPackage ./erldeps/exml.nix {};
    folsom = callPackage ./erldeps/folsom.nix {};
    goldrush = callPackage ./erldeps/goldrush.nix {};
    lager = callPackage ./erldeps/lager.nix {};
    meck = callPackage ./erldeps/meck.nix {};
    mochijson2 = callPackage ./erldeps/mochijson2.nix {};
    ranch = callPackage ./erldeps/ranch.nix {};
    redo = callPackage ./erldeps/redo.nix {};
  };
in self
