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
in stdenv.mkDerivation ({
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
    getDeps = drv: [drv] ++ (map getDeps drv.erlangDeps);
    recursiveDeps = uniqList {
      inputList = flatten (map getDeps erlangDeps);
    };
  in concatMapStringsSep ":" (d: "${d.packageName}=${d}") recursiveDeps;

  buildPhase = ''
    runHook preBuild
    rebar compile
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    for reldir in ebin priv include; do
      [ -e "$reldir" ] || continue
      mkdir -p "$out"
      cp -rt "$out" "$reldir"
      success=1
    done
    runHook postInstall
  '';

  passthru = {
    packageName = name;
    inherit erlangDeps;
  };
} // removeAttrs attrs [ "name" "postPatch" "buildInputs" ])
