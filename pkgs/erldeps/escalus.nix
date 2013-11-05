{ buildErlang, fetchurl, exml, base16, lhttpc, wsecli }:

buildErlang rec {
  name = "escalus";
  version = "2.1.2";

  src = fetchurl {
    url = "https://github.com/esl/escalus/archive/${version}.tar.gz";
    sha256 = "13jsv11990lh3kn5m011274gcfl95zaibnfp1idvbln3ldwwb784";
  };

  postPatch = ''
    sed -i -e '/exml/s|"2\.0\.0"|"2.0.1"|' rebar.config
  '';

  erlangDeps = [ exml base16 lhttpc wsecli ];
}
