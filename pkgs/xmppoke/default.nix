{ stdenv, fetchFromGitHub, fetchFromBitbucket, fetchhg, fetchurl, fetchsvn
, makeWrapper, lua, openssl, libidn, prosody, luaPackages, cacert, unbound
, databaseEngine ? "PostgreSQL", sqlite ? null, postgresql ? null
}:

assert databaseEngine == "PostgreSQL" -> postgresql != null;
assert databaseEngine == "SQLite3"    -> sqlite     != null;

with stdenv.lib;

let
  installPlainLua = ''
    find . -type f -name '*.lua' -print | while read path; do
      mkdir -p "$out/share/lua/${lua.luaversion}/$(dirname "$path")"
      cp -v "$path" "$out/share/lua/${lua.luaversion}/$path"
    done
  '';

  prosodyPoke = stdenv.mkDerivation rec {
    name = "prosody-xmppoke";

    src = fetchhg {
      url = "http://hg.prosody.im/trunk";
      rev = "381e0b874e6db4916054b2d86e54a958a5c4059d";
      sha256 = "09cgilylak2iqz0w41ks9a0ijb1cygk05a8mhyik9r0s0sghbf2h";
    };

    postPatch = ''
      ln -sf "${fetchhg {
        inherit (src) url;
        rev = "bb27ba619932d3af5dcf22a042d7b033d868a39e";
        sha256 = "1kpm81ny6ssl2sxi7vwla5n2zanpxpwj5ql95wpxjjq0j58g30c8";
      }}/util/x509.lua" util/x509.lua
    '';

    buildInputs = [ lua openssl libidn ];

    postInstall = ''
      for newerFile in util/hashes.so util/encodings.so; do
        install -vD "${prosody}/lib/prosody/$newerFile" \
                    "$out/lib/prosody/$newerFile"
      done
    '';
  };

  luaUnboundPoke = stdenv.mkDerivation {
    name = "luaunbound-xmppoke";

    src = fetchhg {
      url = "http://code.zash.se/luaunbound";
      rev = "b4b293593d0ef64d623a54a8b8d2c1dea4c5e870";
      sha256 = "0accfa56fg2rn2gvj24b9cq9da2q61x9hmj2asa97qlpp8i1wp9x";
    };

    postPatch = ''
      rm util.lunbound.lua
      sed -i -re 's/(require *")util\.(lunbound")/\1\2/' *.lua
    '';

    buildInputs = [ lua unbound ];

    makeFlags = [ "LUA_DIR=$(out)" ];

    preInstall = ''
      mkdir -p "$out/lib/lua/${lua.luaversion}"
    '';

    postInstall = ''
      for i in *.lua; do
        install -m 0644 -vD "$i" "$out/share/lua/${lua.luaversion}/''${i/./\/}"
      done

      for i in net.unbound:net/adns fakedns:net/dns; do
        install -m 0644 -vD "''${i%:*}.lua" \
          "$out/share/lua/${lua.luaversion}/''${i#*:}.lua"
      done
    '';
  };

  luaSec = stdenv.mkDerivation {
    name = "luasec-xmppoke";

    src = fetchFromGitHub {
      repo = "luasec";
      owner = "xnyhps";
      rev = "64bebd9c9283ce11dfa60945938d0a529861d824";
      sha256 = "1x7snljlbvnzsify4w4ma76cv493wbjg8f23wagsg38dwqs5wzl9";
    };

    makeFlags = [
      "LUAPATH=$(out)/share/lua/${lua.luaversion}"
      "LUACPATH=$(out)/lib/lua/${lua.luaversion}"
    ];

    buildFlags = [ "linux" ];
    buildInputs = [ lua openssl ];
  };

  luaDbi = stdenv.mkDerivation (rec {
    name = "luadbi-0.5";

    src = fetchhg {
      url = "https://code.google.com/p/luadbi/";
      rev = "47382fea7a9cf6ad067c204f87af968e0f8a6756";
      sha256 = "08dci0lysv38kkzsmwmr34ay7zylwdd67dgha8hgymgixjsgvhff";
    };

    installPhase = ''
      install -m 0644 -vD DBI.lua "$out/share/lua/${lua.luaversion}/DBI.lua"
      runHook postInstall
    '';

    doInstallCheck = true;
    installCheckPhase = ''
      code="print('drivers:'..table.concat(require('DBI').Drivers(), ', '))"
      drivers_available="$(echo "$code" | lua | sed -n -e 's/^drivers://p')"
      if [ "$drivers_available" = '(None)' -o -z "$drivers_available" ]; then
        echo "Unable to load any luaDBI drivers." >&2
        exit 1
      elif [ "$drivers_available" == ${escapeShellArg databaseEngine} ]; then
        echo "Database engines available: $drivers_available" >&2
      else
        echo "Expected ${databaseEngine} but got $drivers_available" >&2
        exit 1
      fi
    '';
  } // (if databaseEngine == "PostgreSQL" then {
    buildFlags = [ "COMMON_LDFLAGS=-L${postgresql.lib}/lib" "psql" ];
    buildInputs = [ lua postgresql ];
    postInstall = ''
      install -vD dbdpostgresql.so \
        "$out/lib/lua/${lua.luaversion}/dbdpostgresql.so"
    '';
  } else if databaseEngine == "SQLite3" then {
    buildFlags = [ "sqlite3" ];
    buildInputs = [ lua sqlite ];
    postInstall = ''
      install -vD dbdsqlite3.so \
        "$out/lib/lua/${lua.luaversion}/dbdsqlite3.so"
    '';
  } else throw "Unsupported database engine ${databaseEngine}."));

  debianBlacklistedSSLCerts = stdenv.mkDerivation rec {
    name = "debian-blacklisted-ssl-certs-${version}";
    version = "354";

    src = fetchsvn {
      url = "svn://svn.debian.org/pkg-openssl/openssl-blacklist/trunk";
      rev = version;
      sha256 = "0cnhnni8s5vii2pbpg9hn84941v6cssb633rc46wjrn7zd6asv9h";
    };

    installPhase = ''
      mkdir -p "$out"
      for keysize in 512 1024 2048 4096; do
        cat debian/blacklist.prefix > "$out/blacklist.RSA-$keysize"
        cat "blacklists/be32/blacklist-$keysize.db" \
            "blacklists/le32/blacklist-$keysize.db" \
            "blacklists/le64/blacklist-$keysize.db" \
            | cut -d ' ' -f 5 | cut -b21- | sort \
            >> "$out/blacklist.RSA-$keysize"
      done
    '';
  };

  verse = stdenv.mkDerivation {
    name = "verse";

    src = fetchhg {
      url = "http://code.matthewwild.co.uk/verse";
      rev = "34b878d58948833baf0d3beee1d00631f09fae75";
      sha256 = "17inbyj5yhlssl3w5hssibndgvd6kgyl7jksi4f47d7n2ky1ncxi";
    };

    installPhase = installPlainLua + ''
      ln -s init.lua "$out/share/lua/${lua.luaversion}/verse.lua"
      ln -s . "$out/share/lua/${lua.luaversion}/verse"
    '';
  };
in stdenv.mkDerivation {
  name = "xmppoke";

  src = fetchFromBitbucket {
    owner = "xnyhps";
    repo = "xmppoke";
    rev = "fbf8af64f6611b32bbc820a18643333d3459fb28";
    sha256 = "0jjmq4yyc5wkwfy4xxhdr9mjv2sc1kkl9mmxjvyb4lkc8q456nhd";
  };

  buildInputs = [ makeWrapper ];

  patches = [ ./server-handler.patch ];

  postPatch = ''
    sed -i -r -e '/^ *print\([^)]*\); *$/d' -e '/^local opts/,/}/ {
      s!^( *cafile *= *)nil!\1"${cacert}/etc/ca-bundle.crt"!
      s!^( *blacklist *= *")[^"]*!\1${debianBlacklistedSSLCerts}!
    }' -e 's/^(local *driver_name *= *)nil/\1"${databaseEngine}"/' poke.lua
  '';

  installPhase = let
    luaPaths = [
      "$out" luaUnboundPoke luaSec verse luaDbi
      luaPackages.luaexpat luaPackages.luabitop luaPackages.luafilesystem
      luaPackages.luasocket
    ];
    luaAbsPaths = [ "${prosodyPoke}/lib/prosody/?.lua" ];
    luaAbsCPaths = [ "${prosodyPoke}/lib/prosody/?.so" ];

    mkPath = base: "${base}/share/lua/${lua.luaversion}/?.lua";
    mkCPath = base: "${base}/lib/lua/${lua.luaversion}/?.so";

    pathString = concatStringsSep ";" (map mkPath luaPaths ++ luaAbsPaths);
    cPathString = concatStringsSep ";" (map mkCPath luaPaths ++ luaAbsCPaths);
  in installPlainLua + ''
    ln -s server_select.lua "$out/share/lua/${lua.luaversion}/net/server.lua"

    mkdir -p "$out/share/lua/${lua.luaversion}/verse/plugins"
    cp -vt "$out/share/lua/${lua.luaversion}/verse/" client.lua server.lua
    cp -vt "$out/share/lua/${lua.luaversion}/verse/plugins/" plugins/tls.lua

    mkdir -p "$out/share/xmppoke"
    cp -vt "$out/share/xmppoke/" schema.pg.sql

    makeWrapper "${lua}/bin/lua $out/share/lua/${lua.luaversion}/poke.lua" \
      "$out/bin/xmppoke" \
      --set LUA_PATH "'${pathString}'" \
      --set LUA_CPATH "'${cPathString}'"
  '';
}
