{ stdenv, fetchFromGitHub, qt4 }:

stdenv.mkDerivation rec {
  name = "libcommuni-${version}";
  version = "3.3.0";

  src = fetchFromGitHub {
    repo = "libcommuni";
    owner = "communi";
    rev = "v${version}";
    sha256 = "1bsblr4rizgjin1p9igymcv40lbkm9p8f4h3k8pwdf6qfxdnzxsx";
  };

  postPatch = ''
    sed -i -e 's|/bin/pwd|pwd|g' -e 's/\<which\>/type -P/g' configure
  '';

  propagatedBuildInputs = [ qt4 ];

  doCheck = true;
}
