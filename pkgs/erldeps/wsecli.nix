{ buildErlang, fetchgit, espec, cucumberl, hamcrest, meck, rebarFeatureRunner
, wsock
}:

buildErlang rec {
  name = "wsecli";
  version = "1";

  src = fetchgit {
    url = "https://github.com/madtrick/wsecli.git";
    rev = "752d062af4fc943414505ce846d5317d003d2dbc";
    sha256 = "0i3bzf7chn6aj7xn8lzf3kh6gy5mr7d6738i0lhfx21y4pyw1xri";
  };

  erlangDeps = [ espec cucumberl hamcrest meck rebarFeatureRunner wsock ];
}
