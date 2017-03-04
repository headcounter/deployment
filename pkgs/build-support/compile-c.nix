{ lib, runCommand }:

{ name, source, cflags ? [] }:

runCommand name { inherit source; } ''
  echo "$source" | gcc -Wall \
    ${lib.concatMapStringsSep " " lib.escapeShellArg cflags} \
    -xc - -o "$out"
''
