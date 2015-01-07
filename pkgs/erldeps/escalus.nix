{ buildErlang, fetchFromGitHub, exml, fusco, base16, lhttpc, wsecli }:

buildErlang rec {
  name = "escalus";
  version = "2.6.0";

  src = fetchFromGitHub {
    owner = "esl";
    repo = "escalus";
    rev = version;
    sha256 = "1p5bdjhdaxhl47z8b8awv0wa6hsm4590d5x61zib2nnphhpblnn7";
  };

  erlangDeps = [ exml fusco base16 lhttpc wsecli ];
}
