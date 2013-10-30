{ buildErlang, fetchgit }:

buildErlang {
  name = "redo";
  version = "1.1.0";
  src = fetchgit {
    url = "https://github.com/JacobVorreuter/redo.git";
    rev = "7c7eaef4cd65271e2fc4ea88587e848407cf0762";
    sha256 = "1i1lgxw5iasf49cvsyfcrv0sv1yxlzf06yi40w8ygd8yid6z1siv";
  };
}
