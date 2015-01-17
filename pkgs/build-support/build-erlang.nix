{ stdenv, erlang, rebar }:

{ name, version
, buildInputs ? [], erlangDeps ? []
, postPatch ? ""
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
      rm -f rebar
      if [ -e "src/${name}.app.src" ]; then
        sed -i \
          -e 's/{ *vsn *,[^}]*}/{vsn, "${version}"}/' \
          -e '/^ *%/s/[^[:print:][:space:]]/?/g' \
          "src/${name}.app.src"
      fi
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

    passthru = rec {
      packageName = name;
      libraryDir = "${self}/lib/erlang/lib";
      appDir = "${libraryDir}/${packageName}";
      inherit erlangDeps recursiveErlangDeps;
    };
  } // removeAttrs attrs [ "name" "postPatch" "buildInputs" ]);

in self
