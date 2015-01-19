{ stdenv, buildErlang, fetchFromGitHub, pam, zlib, openssl, expat
, erlangPackages
}:

buildErlang rec {
  name = "mongooseim";
  version = "1.5.0";

  src = fetchFromGitHub {
    owner = "esl";
    repo = "MongooseIM";
    rev = version;
    sha256 = "0y1690bfiasbrd3l9migywxczncls44hnf7kggxgn7rc1ks5d06j";
  };

  patches = [
    ./reltool.patch ./journald.patch ./systemd.patch
    ./s2s-listener-certfile.patch
  ];

  postPatch = ''
    sed -i -e '/\<vsn\>/s/{ *cmd *,[^}]*}/"2.1.8+mim-${version}"/' \
      apps/ejabberd/src/ejabberd.app.src

    sed -i \
      -e '/lager/s/2\.0\.3/.*/' \
      -e '/cowboy/s/0\.9\.0/.*/' \
      rebar.config

    # Remove dependencies we don't need
    sed -i -re '/\<(mysql|pgsql|redo|seestar|odbc)\>/d' rel/reltool.config
  '';

  buildInputs = [ pam zlib openssl expat ];
  erlangDeps = with erlangPackages; [
    alarms cowboy cuesport exml folsom lager mochijson2 p1_cache_tab
    p1_stringprep redo fusco seestar proper pa ecoveralls
  ];

  postBuild = ''
    rebar generate
  '';

  installPhase = ''
    cp -a "rel/${name}" "$out"
  '';

  meta = {
    homepage = "https://www.erlang-solutions.com/products/"
             + "mongooseim-massively-scalable-ejabberd-platform";
    description = "An Ejabberd fork utilizing Erlang/OTP features.";
    license = stdenv.lib.licenses.gpl2Plus;
  };
}
