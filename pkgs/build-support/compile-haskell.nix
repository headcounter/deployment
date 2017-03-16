{ lib, runCommand, haskellPackages, writeText }:

{ name, source, ghcflags ? [], buildDepends ? [] }:

runCommand name {
  src = if lib.isString source then writeText "${name}.hs" source else source;
  ghc = haskellPackages.ghcWithPackages (p: map (d: p.${d}) buildDepends);
} ''
  "$ghc/bin/ghc" ${lib.concatMapStringsSep " " lib.escapeShellArg ghcflags} \
    --make "$src" -odir . -o "$out"
''
