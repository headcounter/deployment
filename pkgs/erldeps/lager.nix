{ buildErlang, fetchFromGitHub, goldrush }:

buildErlang rec {
  name = "lager";
  version = "2.1.0";

  src = fetchFromGitHub {
    repo = "lager";
    owner = "basho";
    rev = version;
    sha256 = "1bwd4g8bzh9msxzpp5nm4sssh883xycfrp76nk63r3vdah2ypmsv";
  };

  erlangDeps = [ goldrush ];
}
