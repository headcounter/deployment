{ buildErlang, fetchFromGitHub }:

buildErlang {
  name = "p1_utils";
  version = "1";

  src = fetchFromGitHub {
    owner = "processone";
    repo = "p1_utils";
    rev = "9e646e4ff343e8e902410fa1fe28803202b7e340";
    sha256 = "0rlxgw4gsxacihlriv5spdnva88vygpx659m6x8bvqqmd6yhnpgr";
  };
}
