{ stdenv, fetchgit, pkgconfig, scons, boost, openssl, expat, libidn, zlib }:

stdenv.mkDerivation rec {
  name = "swiften-${version}";
  version = "3.0alpha";

  src = fetchgit {
    url = "git://swift.im/swift";
    rev = "refs/tags/swift-${version}";
    sha256 = "0rblfwk33vg1bhy7dh4qkn1xy5nz48rlr5x4ychfyqrb05hrs7ak";
  };

  buildInputs = [ pkgconfig scons boost openssl expat libidn zlib ];

  patches = [ ./swiften.patch ];

  configurePhase = ''
    cat > config.py <<PYTHON
    boost_libdir = "${boost.lib}/lib"
    boost_includedir = "${boost.dev}/include"
    expat_libdir = "${expat}/lib"
    expat_includedir = "${expat}/include"
    libidn_libdir = "${libidn}/lib"
    libidn_includedir = "${libidn}/include"
    zlib_libdir = "${zlib}/lib"
    zlib_includedir = "${zlib}/include"
    openssl = "${openssl}"
    swiften_dll = True
    boost_bundled_enable = False
    libidn_bundled_enable = False
    zlib_bundled_enable = False
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
    LD_LIBRARY_PATH=./Swiften HOME="$(pwd)" \
      scons -j$NIX_BUILD_CORES -l$NIX_BUILD_CORES test=unit QA
  '';
}
