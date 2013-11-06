{ buildErlang, fetchurl, expat }:

buildErlang rec {
  name = "exml";
  version = "2.0.1";

  src = fetchurl {
    url = "https://github.com/esl/exml/archive/${version}.tar.gz";
    sha256 = "101jm1kyqdw799fx5vbbadjx0mk8gwj2aldx79wln20c6kh17v9a";
  };

  postPatch = ''
    sed -i -e 's/exml.so/exml_drv.so/' rebar.config
  '';

  buildInputs = [ expat ];

  postInstall = ''
    ln -s exml_drv.so "$out/priv/exml.so"
  '';
}
