{ buildErlang, fetchurl }:

buildErlang rec {
  name = "meck";
  version = "0.8.1";

  src = fetchurl {
    url = "https://github.com/eproxus/${name}/archive/${version}.tar.gz";
    sha256 = "12h4bxcj0ka6zl7zngnbhly14yfv7aslm30xwx9l9sxrlvcxsfj4";
  };
}
