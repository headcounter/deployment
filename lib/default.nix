{ lib ? import <nixpkgs/lib> }:

let
  hasCredentials = builtins.pathExists ../credentials.nix;
  credAttrs = lib.optionalAttrs hasCredentials {
    credentials = import ../credentials.nix;
  };

in import ./erlang.nix lib //
   import ./module-support.nix lib //
   credAttrs
