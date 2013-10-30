{ buildErlang, fetchurl }:

buildErlang rec {
  name = "ranch";
  version = "0.8.5";

  src = fetchurl {
    url = "https://github.com/extend/ranch/archive/${version}.tar.gz";
    sha256 = "1ngqbnpv398s15b54hlkaq0h5nxd29i6svfjpr9v3p8s9hbz46d0";
  };
}
