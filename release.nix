{ nixpkgs ? <nixpkgs>
}:

let
  pkgs = import nixpkgs { system = "x86_64-linux"; };
  systems = [ "x86_64-linux" ];
in {
  tests = with pkgs.lib; let
    testsOnly = attrs: !attrs ? test;
    testsFor = system: let
      mapper = mapAttrsRecursiveCond testsOnly sysAttrs;
      sysAttrs = _: val: listToAttrs [(nameValuePair system val.test)];
    in mapper (import ./tests {
      inherit nixpkgs system;
    });
  in fold recursiveUpdate {} (map testsFor systems);
}
