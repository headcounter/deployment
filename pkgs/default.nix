{ pkgs ? import <nixpkgs> {} }:

let
  callPackage = pkgs.lib.callPackageWith (pkgs // self);

  self = rec {
    buildErlang = callPackage ./build-support/build-erlang.nix {};

    mongooseim = callPackage ./mongooseim {};
    mongooseimTests = callPackage ./mongooseim/tests.nix {};

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

    # dependencies for mongooseim tests
    base16 = callPackage ./erldeps/base16.nix {};
    cucumberl = callPackage ./erldeps/cucumberl.nix {};
    escalus = callPackage ./erldeps/escalus.nix {};
    espec = callPackage ./erldeps/espec.nix {};
    hamcrest = callPackage ./erldeps/hamcrest.nix {};
    lhttpc = callPackage ./erldeps/lhttpc.nix {};
    mustache = callPackage ./erldeps/mustache.nix {};
    rebarFeatureRunner = callPackage ./erldeps/rebar-feature-runner.nix {};
    reloader = callPackage ./erldeps/reloader.nix {};
    wsecli = callPackage ./erldeps/wsecli.nix {};
    wsock = callPackage ./erldeps/wsock.nix {};

    xmppoke = callPackage ./xmppoke {
      lua = pkgs.lua5;
    };
  };
in self
