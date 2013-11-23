{ stdenv, fetchurl, qt4 }:

stdenv.mkDerivation rec {
  name = "libcommuni-${version}";
  version = "2.2.0";

  src = fetchurl {
    url = "https://github.com/communi/libcommuni/archive/v${version}.tar.gz";
    sha256 = "1ir8kwjlfwbb87fdmki6dwmd61xraj7c0hami8yzgdz0nxi5kfsw";
  };

  postPatch = ''
    sed -i -e 's|/bin/pwd|pwd|' -e 's/\<which\>/type -P/' configure
  '';

/* Only needed for 3.x
  preConfigure = ''
    configureFlags="-importdir $out/lib/qt4/imports"
  '';
*/

  propagatedBuildInputs = [ qt4 ];

  doCheck = true;
}
