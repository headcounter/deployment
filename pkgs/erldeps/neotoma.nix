{ buildErlang, fetchFromGitHub }:

buildErlang rec {
  name = "neotoma";
  version = "1.7.2";

  src = fetchFromGitHub {
    owner = "seancribbs";
    repo = "neotoma";
    rev = version;
    sha256 = "0h4q5k8fb4l9iqkqsza8jpzg7pzprjx16pkfyz5fxbh02xig1r06";
  };
}
