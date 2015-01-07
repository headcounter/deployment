{ buildErlang, fetchFromGitHub, edown }:

buildErlang {
  name = "seestar";
  version = "0.0.1";

  src = fetchFromGitHub {
    repo = "seestar";
    owner = "iamaleksey";
    rev = "94b17823f182fef20f878b19ea20761c00795879";
    sha256 = "13l47hj7lm9ciqyk0rk3pzllj12141jcqkr5j7hpnwg44j4xd8wm";
  };

  erlangDeps = [ edown ];
}
