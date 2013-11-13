{ stdenv, buildErlang, fetchgit, pam, zlib, openssl, expat
, cuesport, redo, exml, lager, cowboy, folsom, mochijson2, alarms
}:

buildErlang rec {
  name = "esl-ejabberd";
  version = "1.2.2-git";

  src = fetchgit {
    url = "https://github.com/esl/ejabberd.git";
    rev = "b09bcbedc0d2826ccbd9ad1b45558d7b9f95d6a3";
    sha256 = "193jw4rdrk88gkrmhzgycjfmh0k5vcmnx50w4jp2b063w423lagr";
  };

  patches = [ ./reltool.patch ./journald.patch ./systemd.patch ];

  buildInputs = [ pam zlib openssl expat ];
  erlangDeps = [ cuesport redo exml lager cowboy folsom mochijson2 alarms ];

  postBuild = ''
    rebar generate
  '';

  installPhase = ''
    cp -a rel/ejabberd "$out"
  '';

  meta = {
    homepage = "https://www.erlang-solutions.com/products/"
             + "mongooseim-massively-scalable-ejabberd-platform";
    description = "An Ejabberd fork utilizing Erlang/OTP features.";
    license = stdenv.lib.licenses.gpl2Plus;
  };
}
