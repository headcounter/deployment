{ stdenv, buildErlang, fetchFromGitHub, pam, zlib, openssl, expat
, erlangPackages
}:

let
  self = buildErlang rec {
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
      ./s2s-listener-certfile.patch ./starttls.patch
    ];

    buildInputs = [ pam zlib openssl expat ];
    erlangDeps = with erlangPackages; [
      alarms base16 cowboy cuesport ecoveralls exml folsom fusco idna lager
      mochijson2 mustache p1_cache_tab p1_stringprep pa proper recon redo
      seestar
    ];

    postBuild = ''
      rebar generate

      # Remove nodetool and the erl wrapper, because we don't need it
      # but be extra paranoid about the deletion. We want deletion to
      # fail if there are extra files left or if a file does not exist.
      basedir="$(echo "rel/${name}"/erts-[0-9]*)"
      for to_delete in nodetool erl; do
        rm "$basedir/bin/$to_delete" || exit 1
      done
      rmdir "$basedir/bin" || exit 1
      rmdir "$basedir" || exit 1
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
