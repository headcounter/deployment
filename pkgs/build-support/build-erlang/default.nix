{ stdenv, erlang, rebar, writeEscript }:

{ name, version
, buildInputs ? [], erlangDeps ? []
, postPatch ? ""
, passthru ? {}
, ... }@attrs:

with stdenv.lib;

let
  patchedRebar = stdenv.lib.overrideDerivation rebar (rb: {
    patches = [ ./rebar-nix.patch ];
  });

  recursiveErlangDeps = let
    getDeps = drv: [drv] ++ (map getDeps drv.erlangDeps);
    inputList = flatten (map getDeps erlangDeps);
  in uniqList { inherit inputList; };

  self = stdenv.mkDerivation ({
    name = "${name}-${version}";

    buildInputs = buildInputs ++ [ erlang patchedRebar ];

    postPatch = ''
      "${writeEscript "rewrite-rebar-config" [] ./rewrite-rebar-config.erl}" \
        rebar.config rebar.config.script || :

      subappfiles="$(
        for subapp in apps/*; do
          if [ -e "$subapp" ]; then
            echo "$subapp/src/$(basename "$subapp").app.src"
          fi
        done
      )"

      rm -f rebar
      hashver="$(basename "$out" | cut -d- -f1)"
      for appfile in "src/${name}.app.src" "ebin/${name}.app" $subappfiles; do
        if [ -e "$appfile" ]; then
          sed -i \
            -e 's/{ *vsn *,[^}]*}/{vsn, "'"$hashver"'"}/' \
            -e '/^ *%/s/[^[:print:][:space:]]/?/g' \
            "$appfile"
        fi
      done

      ${postPatch}
    '';

    configurePhase = ''
      runHook preConfigure
      runHook postConfigure
    '';

    NIX_ERLANG_DEPENDENCIES = let
      mkDepMapping = d: "${d.packageName}=${d.appDir}";
    in concatMapStringsSep ":" mkDepMapping recursiveErlangDeps;

    buildPhase = ''
      runHook preBuild
      rebar compile
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      for reldir in ebin priv include; do
        [ -e "$reldir" ] || continue
        mkdir -p "$out/lib/erlang/lib/${name}"
        cp -rt "$out/lib/erlang/lib/${name}" "$reldir"
      done
      runHook postInstall
    '';

    passthru = passthru // rec {
      packageName = name;
      libraryDir = "${self}/lib/erlang/lib";
      appDir = "${libraryDir}/${packageName}";
      inherit erlangDeps recursiveErlangDeps;
    };
  } // removeAttrs attrs [ "name" "postPatch" "buildInputs" "passthru" ]);

in self
