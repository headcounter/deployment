{ buildErlang, fetchgit }:

buildErlang rec {
  name = "hamcrest";
  version = "0.1.0";

  src = fetchgit {
    url = "https://github.com/hyperthunk/hamcrest-erlang.git";
    rev = "7215234e14a7c82458829c542edbf2899ceedbd3";
    sha256 = "0v6fg8bbvv1cx5qjff6l880r1b9fgrr8czrxw2bignw5mbrfdlx9";
  };
}
