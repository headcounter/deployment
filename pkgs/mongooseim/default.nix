{ stdenv, buildErlang, fetchgit, pam, zlib, openssl, expat
, cuesport, redo, exml, lager, cowboy, folsom, mochijson2, alarms
}:

buildErlang rec {
  name = "esl-ejabberd";
  version = "1.2.2-git";

  src = fetchgit {
    url = "https://github.com/esl/MongooseIM.git";
    rev = "301303b3d232b88d257c61a2611bc0bd56ce0d0f";
    sha256 = "19nc90mbhl178k9bbqm2pycz4g82jlfx0zxz74zg0c39v71l77f8";
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
