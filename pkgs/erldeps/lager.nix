{ buildErlang, fetchurl, goldrush }:

buildErlang rec {
  name = "lager";
  version = "2.0.0";

  src = fetchurl {
    url = "https://github.com/basho/lager/archive/${version}.tar.gz";
    sha256 = "0qhijdbags4izh1bpvqpwrlbq2al1nf0kj0v5gmvbkj2rgm7dzb6";
  };

  erlangDeps = [ goldrush ];
}
