{ pkgs ? import ../pkgs {}
, system ? builtins.currentSystem
}:

let
  callTest = fn: args: import fn ({
    inherit (pkgs) lib;
    inherit pkgs system;
  } // args);
in {
  acme = callTest ./acme.nix {};
  code-reload = callTest ./code-reload {};
  dyndns = callTest ./dyndns.nix {};
  hclib = pkgs.callPackage ./hclib.nix {};
  headcounter = callTest ./headcounter {};
  mongooseim = callTest ./mongooseim {};
  nsd-zone-writer = callTest ./nsd-zone-writer.nix {};
  postfix = callTest ./postfix.nix {};
  postgresql = callTest ./postgresql.nix {};
}
