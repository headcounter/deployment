{ stdenv, fetchFromGitHub, fetchhg, fetchurl, makeWrapper, lua, openssl, libidn
, sqlite, expat, prosody, luaPackages, cacert }:

with stdenv.lib;

let
  installPlainLua = ''
    find . -type f -name '*.lua' -print | while read path; do
      mkdir -p "$out/share/lua/${lua.luaversion}/$(dirname "$path")"
      cp -v "$path" "$out/share/lua/${lua.luaversion}/$path"
    done
  '';

  luaSec = stdenv.mkDerivation {
    name = "luasec-prosody-0.5";

    src = fetchFromGitHub {
      repo = "luasec";
      owner = "xnyhps";
      rev = "a28dcbab5b5bcc81705bfae2fc5e462be5a05683";
      sha256 = "1n8gvazz6471n0q9hc8wdgvbjzpwp27x17smi7dcpjf1ppsdyyg6";
    };

    makeFlags = [
      "LUAPATH=$(out)/share/lua/${lua.luaversion}"
      "LUACPATH=$(out)/lib/lua/${lua.luaversion}"
    ];

    buildFlags = [ "linux" ];
    buildInputs = [ lua openssl ];
  };

  luaDbi = stdenv.mkDerivation rec {
    name = "luadbi-0.5";

    src = fetchurl {
      url = "https://luadbi.googlecode.com/files/luadbi.0.5.tar.gz";
      sha256 = "07ikxgxgfpimnwf7zrqwcwma83ss3wm2nzjxpwv2a1c0vmc684a9";
    };

    setSourceRoot = "sourceRoot=.";
    buildInputs = [ lua sqlite ];
    buildFlags = [ "sqlite3" ];

    installPhase = ''
      install -m 0644 -vD DBI.lua "$out/share/lua/${lua.luaversion}/DBI.lua"
      install -vD dbdsqlite3.so "$out/lib/lua/${lua.luaversion}/dbdsqlite3.so"
    '';
  };

  verse = stdenv.mkDerivation {
    name = "verse";

    src = fetchhg {
      url = "http://code.matthewwild.co.uk/verse";
      rev = "154c2f04d73b0a3ba1d4e0b12295c804f0fc3927";
      sha256 = "1x92jly2g72sjsvhmvm7hxmms4y7pj53fy8k1qin86ky11gbg7rh";
    };

    installPhase = installPlainLua + ''
      ln -s init.lua "$out/share/lua/${lua.luaversion}/verse.lua"
      ln -s . "$out/share/lua/${lua.luaversion}/verse"
    '';
  };
in stdenv.mkDerivation {
  name = "xmppoke";

  src = fetchhg {
    url = "https://bitbucket.org/xnyhps/xmppoke";
    rev = "7acb1a8d622787bad2f6a21ea00ea89fbc87c3b7";
    sha256 = "12l83zp0z00m5gz4d6nr44jqzm2b9jkbjajzs0vb2mwgzjrd52b1";
  };

  buildInputs = [ makeWrapper ];

  postPatch = ''
    sed -i -r -e 's/(driver_name = ")[^"]*/\1SQLite3/p' poke.lua
  '';

  installPhase = let
    luaPaths = [
      "$out" luaSec verse luaDbi
      luaPackages.luaexpat luaPackages.luabitop luaPackages.luafilesystem
      luaPackages.luasocket
    ];
    luaAbsPaths = [ "${prosody}/lib/prosody/?.lua" ];
    luaAbsCPaths = [ "${prosody}/lib/prosody/?.so" ];

    mkPath = base: "${base}/share/lua/${lua.luaversion}/?.lua";
    mkCPath = base: "${base}/lib/lua/${lua.luaversion}/?.so";

    pathString = concatStringsSep ";" (map mkPath luaPaths ++ luaAbsPaths);
    cPathString = concatStringsSep ";" (map mkCPath luaPaths ++ luaAbsCPaths);
  in installPlainLua + ''
    mkdir -p "$out/share/lua/${lua.luaversion}/net"
    ln -s "${prosody}/lib/prosody/net/server_select.lua" \
      "$out/share/lua/${lua.luaversion}/net/server.lua"

    mkdir -p "$out/share/xmppoke"
    cp -v schema.sqlite3.sql "$out/share/xmppoke/schema.sql"

    makeWrapper "${lua}/bin/lua $out/share/lua/${lua.luaversion}/poke.lua" \
      "$out/bin/xmppoke" \
      --set LD_LIBRARY_PATH "${makeLibraryPath [ expat openssl ]}" \
      --set SSL_CERT_FILE "${cacert}/etc/ca-bundle.crt" \
      --set LUA_PATH "'${pathString}'" \
      --set LUA_CPATH "'${cPathString}'"
  '';
}
