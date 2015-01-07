{ buildErlang, fetchFromGitHub }:

buildErlang rec {
  name = "edown";
  version = "0.4";

  src = fetchFromGitHub {
    repo = "edown";
    owner = "esl";
    rev = version;
    sha256 = "0sq7hc7mmcv8maxg2map9czr2772gsbrjsr1gffk7r5f12lc7ffv";
  };
}
