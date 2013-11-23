{ stdenv, fetchurl, python, postgresql }:

stdenv.mkDerivation rec {
  name = "libpqxx-${version}";
  version = "4.0.1";

  src = fetchurl {
    url = "http://pqxx.org/download/software/libpqxx/${name}.tar.gz";
    sha256 = "0f6wxspp6rx12fkasanb0z2g2gc8dhcfwnxagx8wwqbpg6ifsz09";
  };

  postPatch = ''
    sed -i -e 's|/bin/true|true|' configure
    sed -i -e 's|/usr/bin/python|${python}/bin/python|' tools/splitconfig
  '';

  configureFlags = "--enable-shared";

  propagatedBuildInputs = [ postgresql ];
}
