{ buildErlang, fetchurl, bear, meck }:

buildErlang rec {
  name = "folsom";
  version = "0.7.4";

  src = fetchurl {
    url = "https://github.com/boundary/${name}/archive/${version}.tar.gz";
    sha256 = "0dg62i92lfc2k2p4zn78w61iiw09fm0pamx11ljb6c0rpkqwx8jv";
  };

  erlangDeps = [ bear meck ];
}
