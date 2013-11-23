{ stdenv, fetchgit, pkgconfig, scons, boost, openssl, expat, libidn, zlib }:

stdenv.mkDerivation rec {
  name = "swiften-2.0git";

  src = fetchgit {
    url = "git://swift.im/swift";
    rev = "68a59bf7ef72857d730e650887355911bc72c6e6";
    sha256 = "06hk391pgkz6kkfjfgzks8y8dm7xb92nlhsrkkzqs1iid10v5ram";
  };

  # TODO: Doesn't detect zlib!
  buildInputs = [ pkgconfig scons boost openssl expat libidn zlib ];

  configurePhase = ''
    cat > config.py <<PYTHON
    boost_libdir = "${boost}/lib"
    boost_includedir = "${boost}/include"
    expat_libdir = "${expat}/lib"
    expat_includedir = "${expat}/include"
    libidn_libdir = "${libidn}/lib"
    libidn_includedir = "${libidn}/include"
    openssl = "${openssl}"
    swiften_dll = True
    PYTHON
  '';

  buildPhase = ''
    scons -j$NIX_BUILD_CORES -l$NIX_BUILD_CORES Swiften
  '';

  installPhase = ''
    scons "SWIFTEN_INSTALLDIR=$out" "$out"
  '';

  doCheck = true;
  checkPhase = ''
    scons -j$NIX_BUILD_CORES -l$NIX_BUILD_CORES test=unit Swiften
  '';
}
