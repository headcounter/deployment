{ buildErlang, fetchFromGitHub, p1Utils }:

buildErlang {
  name = "p1_cache_tab";
  version = "0.1.0";

  src = fetchFromGitHub {
    owner = "processone";
    repo = "cache_tab";
    rev = "7b89d6afb66d8ff9d56671864be74654f5b18e2f";
    sha256 = "1hw3hgzddcanzs6w88n66j2kdyz44zjayjwc3pg88bcr4rcwx46f";
  };

  erlangDeps = [ p1Utils ];
}
