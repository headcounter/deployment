{ buildErlang, fetchurl, ranch }:

buildErlang rec {
  name = "cowboy";
  version = "0.8.6";

  src = fetchurl {
    url = "https://github.com/extend/cowboy/archive/${version}.tar.gz";
    sha256 = "1vl5kwp892m87yz3gnzw0ns7162p03q1j5zv05jf75fvx3vydx0j";
  };

  erlangDeps = [ ranch ];
}
