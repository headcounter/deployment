{ buildErlang, fetchgit, reloader }:

buildErlang rec {
  name = "espec";
  version = "1";

  src = fetchgit {
    url = "https://github.com/lucaspiller/${name}.git";
    rev = "44dd72b8924425f09ad1093226ce0c755524a507";
    sha256 = "1jd7kkmmzgrb5x09ga7vmnq600jsc9m25i9x97wlnvjdk7fghjfx";
  };

  erlangDeps = [ reloader ];
}
