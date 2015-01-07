{ buildErlang, fetchFromGitHub, proper }:

buildErlang rec {
  name = "pa";
  version = "0.2.0";

  src = fetchFromGitHub {
    repo = "pa";
    owner = "lavrin";
    rev = version;
    sha256 = "1kzh2g71sim98jd03xh697s8q0az0ma2p2inqc8cwhhr1lyfj2yp";
  };

  erlangDeps = [ proper ];
}
