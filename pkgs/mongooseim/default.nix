{ stdenv, buildErlang, fetchFromGitHub, pam, zlib, openssl, expat
, erlangPackages
}:

let
  self = buildErlang rec {
    name = "mongooseim";
    version = "1.6.0";

    src = fetchFromGitHub {
      owner = "esl";
      repo = "MongooseIM";
      rev = version;
      sha256 = "1l7xnz268zl059xvvyxvvvyhncdi68s9gla7ay6gl99w6jxa6lfk";
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
      find apps \( -iname '*pgsql*' -o -iname '*mysql*' -o -iname '*riak*' \) \
        -type f -delete
    '';

    buildInputs = [ pam zlib openssl expat ];
    erlangDeps = with erlangPackages; [
      alarms base16 cowboy cuesport ecoveralls exml exometer folsom fusco idna
      lager mochijson2 mustache p1_cache_tab p1_stringprep pa proper recon redo
    ];

    postBuild = ''
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
