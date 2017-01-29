{ stdenv, buildErlang, fetchFromGitHub, pam, zlib, expat
, erlangPackages
}:

let
  self = buildErlang rec {
    name = "mongooseim";
    version = "2.0.1";

    src = fetchFromGitHub {
      owner = "esl";
      repo = "MongooseIM";
      rev = version;
      sha256 = "18j2chhvq1pm78ay3gb19yjqifdyzxgbjz8hycjp529sxji3fdzq";
    };

    patches = [
      ./reltool.patch ./journald.patch ./systemd.patch
      ./s2s-listener-certfile.patch ./strip-unneeded-deps.patch
    ];

    prePatch = ''
      sed -i -e 's/{vsn, {cmd, [^}]*}}/{vsn, "2.1.8+mim-${version}"}/' \
        apps/ejabberd/src/ejabberd.app.src
    '';

    postPatch = ''
      rm -rf apps/{pgsql,mysql}
      find apps \( \
        -iname '*pgsql*' -o \
        -iname '*mysql*' -o \
        -iname '*riak*'  -o \
        -iname '*cassandra*' \) \
        -type f -delete
      patchShebangs tools/configure
    '';

    postConfigure = "./tools/configure with-none";

    buildInputs = [ pam zlib expat ];
    erlangDeps = with erlangPackages; [
      alarms base16 cache_tab cowboy cuesport ecoveralls exml exometer fast_tls
      folsom fusco idna jiffy lager lasse mochijson2 mustache pa proper poolboy
      recon redo sd_notify stringprep usec uuid
    ];

    postBuild = ''
      make rel/vars.config
      rebar generate
    '';

    installPhase = ''
      cp -a "rel/${name}" "$out"

      hashver="$(basename "$out" | cut -d- -f1)_2.1.8+mim-${version}"
      mainAppDir="$out/lib/ejabberd-$hashver"
      if [ ! -d "$mainAppDir" ]; then
        echo "$mainAppDir does not exist!" >&2
        exit 1
      fi

      mkdir "$out/nix-support"
      echo -n "$mainAppDir" > "$out/nix-support/main-app-dir"
    '';

    passthru.mainAppDir = builtins.readFile "${self}/nix-support/main-app-dir";

    meta = {
      homepage = "https://www.erlang-solutions.com/products/"
               + "mongooseim-massively-scalable-ejabberd-platform";
      description = "An Ejabberd fork utilizing Erlang/OTP features.";
      license = stdenv.lib.licenses.gpl2Plus;
    };
  };

in self
