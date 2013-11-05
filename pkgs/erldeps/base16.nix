{ buildErlang, fetchgit }:

buildErlang rec {
  name = "base16";
  version = "0.1";

  src = fetchgit {
    url = "https://github.com/goj/base16.git";
    rev = "ec420aa4ce0fb971f155274c606e00188a2ed37d";
    sha256 = "0wlxqb8w6nimjy5w9njmhmn8nn7jhsgv3y6fz3m0zr0iabpgiigy";
  };
}
