{ buildErlang, fetchgit, meck }:

buildErlang rec {
  name = "mustache";
  version = "0.1.0";

  src = fetchgit {
    url = "https://github.com/mojombo/mustache.erl.git";
    rev = "c0154ce140d7c5b088eee6aaa05226b388583ef4";
    sha256 = "0hasbajn7sdz5x3j3q66qbyd1ffgdg5gm8iw1kkkj8811d851afq";
  };

  erlangDeps = [ meck ];
}
