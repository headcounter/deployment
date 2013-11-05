{ buildErlang, fetchurl }:

buildErlang rec {
  name = "wsock";
  version = "1.0.2";

  src = fetchurl {
    url = "https://github.com/madtrick/wsock/archive/${version}.tar.gz";
    sha256 = "1sch3g2x01a346bmasgz09k2m66nridkdz7xqi2i846ng4440njs";
  };
}
