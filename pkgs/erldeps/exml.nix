{ buildErlang, fetchFromGitHub, expat }:

buildErlang rec {
  name = "exml";
  version = "2.1.5";

  src = fetchFromGitHub {
    owner = "esl";
    repo = "exml";
    rev = version;
    sha256 = "1qmixn7i4gvc080pvhy9c0pwlswyshnghwavg4y36x1sl8rhcv7g";
  };

  buildInputs = [ expat ];
}
