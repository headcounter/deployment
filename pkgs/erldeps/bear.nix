{ buildErlang, fetchurl }:

buildErlang rec {
  name = "bear";
  version = "0.1.3";

  src = fetchurl {
    url = "https://github.com/boundary/${name}/archive/${version}.tar.gz";
    sha256 = "0za2rg6vchb5c30lbhwsz71ylrjr5ms20ns0vgs5jhlaj5j7bxbd";
  };
}
