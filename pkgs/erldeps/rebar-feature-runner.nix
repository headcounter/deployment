{ buildErlang, fetchgit }:

buildErlang rec {
  name = "rebar_feature_runner";
  version = "0.1";

  src = fetchgit {
    url = "https://github.com/madtrick/rebar_feature_runner.git";
    rev = "bcbf1ba233a5f8388f6d530c707d98db2021a48a";
    sha256 = "1qi9lkkj0w86w2adn7mj119p6dn6yjvm61zhzmb5qj330gg2k1s9";
  };
}
