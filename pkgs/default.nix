{ pkgs ? import <nixpkgs> {} }:

let
  callPackage = pkgs.lib.callPackageWith (pkgs // headcounter);

  headcounter = rec {
    buildErlang = callPackage ./build-support/build-erlang {};
    writeEscript = callPackage ./build-support/write-escript.nix {};

    nixErlangTools = callPackage ./build-support/liberlang.nix {};

    mongooseim = callPackage ./mongooseim {};
    mongooseimTests = callPackage ./mongooseim/tests.nix {};
    spectrum2 = callPackage ./spectrum2 {};

    haxe = callPackage ./haxe {};
    site = callPackage ./site {};

    erlangPackages = callPackage ./erlang-packages.nix {
      inherit pkgs buildErlang writeEscript;
    };

    # dependencies for spectrum2
    libcommuni = callPackage ./spectrum2/libcommuni.nix {};
    libpqxx = callPackage ./spectrum2/libpqxx.nix {};
    swiften = callPackage ./spectrum2/swiften.nix {};

    xmppoke = callPackage ./xmppoke {
      lua = pkgs.lua5_1;
      luaPackages = pkgs.lua51Packages;
    };
    xmppokeReport = callPackage ./xmppoke/genreport.nix {};
  };
in pkgs // {
  inherit headcounter;
}
