{ pkgs ? import <nixpkgs> {} }:

let
  callPackage = pkgs.lib.callPackageWith (pkgs // self);

  self = rec {
    buildErlang = callPackage ./build-support/build-erlang.nix {};

    mongooseim = callPackage ./mongooseim {};
    mongooseimTests = callPackage ./mongooseim/tests.nix {};
    spectrum2 = callPackage ./spectrum2 {};

    haxe = callPackage ./haxe {};
    site = callPackage ./site {};

    # dependencies for mongooseim
    alarms = callPackage ./erldeps/alarms.nix {};
    bear = callPackage ./erldeps/bear.nix {};
    cowboy = callPackage ./erldeps/cowboy.nix {};
    cowlib = callPackage ./erldeps/cowlib.nix {};
    cuesport = callPackage ./erldeps/cuesport.nix {};
    ecoveralls = callPackage ./erldeps/ecoveralls.nix {};
    edown = callPackage ./erldeps/edown.nix {};
    exml = callPackage ./erldeps/exml.nix {};
    folsom = callPackage ./erldeps/folsom.nix {};
    fusco = callPackage ./erldeps/fusco.nix {};
    goldrush = callPackage ./erldeps/goldrush.nix {};
    jsx = callPackage ./erldeps/jsx.nix {};
    katt = callPackage ./erldeps/katt.nix {};
    lager = callPackage ./erldeps/lager.nix {};
    meck = callPackage ./erldeps/meck.nix {};
    mochijson2 = callPackage ./erldeps/mochijson2.nix {};
    mochijson3 = callPackage ./erldeps/mochijson3.nix {};
    neotoma = callPackage ./erldeps/neotoma.nix {};
    p1CacheTab = callPackage ./erldeps/p1-cache-tab.nix {};
    p1Utils = callPackage ./erldeps/p1-utils.nix {};
    p1Stringprep = callPackage ./erldeps/p1-stringprep.nix {};
    pa = callPackage ./erldeps/pa.nix {};
    proper = callPackage ./erldeps/proper.nix {};
    ranch = callPackage ./erldeps/ranch.nix {};
    redo = callPackage ./erldeps/redo.nix {};
    seestar = callPackage ./erldeps/seestar.nix {};

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

    # dependencies for spectrum2
    libcommuni = callPackage ./spectrum2/libcommuni.nix {};
    libpqxx = callPackage ./spectrum2/libpqxx.nix {};
    swiften = callPackage ./spectrum2/swiften.nix {};

    xmppoke = callPackage ./xmppoke {
      lua = pkgs.lua5;
    };
  };
in self
