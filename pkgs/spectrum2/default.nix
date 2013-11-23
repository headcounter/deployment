{ stdenv, fetchgit, cmake, boost, swiften, popt, sqlite, libpqxx, pidgin
, protobuf, dbus_glib, libev, libcommuni, curl, log4cxx }:

stdenv.mkDerivation {
  name = "spectrum2-git";

  src = fetchgit {
    url = "https://github.com/hanzz/libtransport.git";
    rev = "51746c5af7dfb611f2d16129eaec50efae2289f1";
    sha256 = "1ww1hnhfygcq6jxa0kfzdmvfx7pnxakwn0z2nym64g6i0vnndf79";
  };

  postPatch = ''
    sed -i \
      -e 's|^FIND_LIBRARY.*|SET(IRC_LIBRARY ${libcommuni}/lib/libCommuni.so)|' \
      -e 's|^FIND_PATH.*|SET(IRC_INCLUDE_DIR ${libcommuni}/include)|' \
      cmake_modules/CommuniConfig.cmake
    sed -i -e 's|/etc|'"$out"'/etc|' \
      spectrum/src/CMakeLists.txt spectrum_manager/src/CMakeLists.txt
  '';

  cmakeFlags = [
    "-DENABLE_MYSQL=OFF"
    "-DIRC_LIBRARY=${libcommuni}/lib"
    "-DIRC_INCLUDE_DIR=${libcommuni}/include"
  ];

  enableParallelBuilding = true;

  buildInputs = [
    cmake boost swiften popt sqlite libpqxx pidgin
    protobuf dbus_glib libev libcommuni curl log4cxx
  ];
}
