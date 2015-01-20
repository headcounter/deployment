{ stdenv, fetchFromGitHub, fetchpatch, cmake, boost, swiften, popt, sqlite
, libpqxx, pidgin, protobuf, dbus_glib, libev, libcommuni, curl, log4cxx
}:

stdenv.mkDerivation {
  name = "spectrum2-git";

  src = fetchFromGitHub {
    repo = "libtransport";
    owner = "hanzz";
    rev = "a319e79528df9f12cbba699384cd503e17714ca2";
    sha256 = "01ssyzbdf5p84lvdy0957kyiv5rvlm97x7qsbwcj4yf6kajxlxbi";
  };

  patches = stdenv.lib.singleton (fetchpatch {
    url = "https://github.com/jpnurmi/libtransport/compare/libcommuni3.diff";
    sha256 = "1d004n9czyidf93lk7ql4xj9d8xi1d35msbb3m11jsdirrkc9dgl";
  });

  postPatch = ''
    sed -i -e 's|/etc|'"$out"'/etc|' \
      spectrum/src/CMakeLists.txt spectrum_manager/src/CMakeLists.txt
  '';

  cmakeFlags = let
    mkFlags = name: let
      ucName = stdenv.lib.toUpper name;
    in [
      "-DIRC_${ucName}_INCLUDE_DIR=${libcommuni}/include/Irc${name}"
      "-DIRC_${ucName}_LIBRARY=${libcommuni}/lib/libIrc${name}.so"
    ];
    communiFlags = map mkFlags [ "Core" "Model" "Util" ];
  in [ "-DENABLE_MYSQL=OFF" ] ++ stdenv.lib.flatten communiFlags;

  enableParallelBuilding = true;

  buildInputs = [
    cmake boost swiften popt sqlite libpqxx pidgin
    protobuf dbus_glib libev libcommuni curl log4cxx
  ];
}
