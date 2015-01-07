{ buildErlang, fetchFromGitHub, jsx }:

buildErlang rec {
  name = "ecoveralls";
  version = "0.1";

  src = fetchFromGitHub {
    repo = "ecoveralls";
    owner = "nifoc";
    rev = "0e52c4709f763d512e6972e91330977cfedb3d13";
    sha256 = "0p5apdzfncn60rkg7lvn2dvkqh0jcqiq7ba177lvccw7grvmnd0s";
  };

  erlangDeps = [ jsx ];
}
