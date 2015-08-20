{ stdenv, buildErlang, fetchFromGitHub, pam, zlib, openssl, expat
, erlangPackages
}:

buildErlang rec {
  name = "mongooseim";
  version = "1.5.1";

  src = fetchFromGitHub {
    owner = "esl";
    repo = "MongooseIM";
    rev = version;
    sha256 = "0lpia21kmy9im1c0d8zx0i9swdiifypzj1k4xc3drp8kssz9adrz";
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
  '';

  buildInputs = [ pam zlib openssl expat ];
  erlangDeps = with erlangPackages; [
    alarms base16 cowboy cuesport ecoveralls exml folsom fusco idna lager
    mochijson2 mustache p1_cache_tab p1_stringprep pa proper recon redo seestar
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
