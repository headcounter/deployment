{ lib, runCommandCC }:

{ name, source, cflags ? [] }:

runCommandCC name { inherit source; } ''
  echo "$source" | gcc -Wall \
    ${lib.concatMapStringsSep " " lib.escapeShellArg cflags} \
    -xc - -o "$out"
''
