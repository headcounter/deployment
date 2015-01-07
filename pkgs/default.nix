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

    erlangPackages = callPackage ./erlang-packages.nix {
      inherit pkgs buildErlang;
    };

    # dependencies for spectrum2
    libcommuni = callPackage ./spectrum2/libcommuni.nix {};
    libpqxx = callPackage ./spectrum2/libpqxx.nix {};
    swiften = callPackage ./spectrum2/swiften.nix {};

    xmppoke = callPackage ./xmppoke {
      lua = pkgs.lua5;
    };
  };
in self
