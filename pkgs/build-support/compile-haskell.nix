{ lib, runCommand, haskellPackages, writeText }:

{ name, source, ghcflags ? [], buildDepends ? [] }:

let
  mkDep = hpkgs: dep: if lib.isString dep then hpkgs.${dep} else dep;
in runCommand name {
  src = if lib.isString source then writeText "${name}.hs" source else source;
  ghc = haskellPackages.ghcWithPackages (p: map (mkDep p) buildDepends);
} ''
  "$ghc/bin/ghc" ${lib.concatMapStringsSep " " lib.escapeShellArg ghcflags} \
    --make "$src" -odir . -o "$out"
''
