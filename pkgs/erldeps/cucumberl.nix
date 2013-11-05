{ buildErlang, fetchgit }:

buildErlang rec {
  name = "cucumberl";
  version = "0.0.5";

  src = fetchgit {
    url = "https://github.com/madtrick/cucumberl.git";
    rev = "3f2cca66ed87a53a64177232428ffde606bdcb9a";
    sha256 = "0ycprism4hxsnq08hal5wv2fmjizcaxb4i2mz7a84gyh98qb2rkb";
  };

  postPatch = ''
    sed -i -e 's/git/"0.0.5"/' examples/*/src/*.app.src
  '';
}
