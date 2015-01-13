{ stdenv, fetchgit, fetchhg, fetchurl, makeWrapper, lua, openssl, libidn
, sqlite }:

with stdenv.lib;

let
  luaVersion = "5.1";

  installPlainLua = ''
    find . -type f -name '*.lua' -print | while read path; do
      mkdir -p "$out/share/lua/${luaVersion}/$(dirname "$path")"
      cp -v "$path" "$out/share/lua/${luaVersion}/$path"
    done
  '';

  prosody = stdenv.mkDerivation rec {
    name = "prosody-${version}";
    version = "0.9.1";

    src = fetchurl {
      url = "http://prosody.im/downloads/source/${name}.tar.gz";
      sha256 = "1m9cyg4janbqnzxinckb0rgalaashb50jmwrywhwdgi7c3ysdpkc";
    };

    buildInputs = [ lua openssl libidn ];
  };

  luaSec = stdenv.mkDerivation {
    name = "luasec-prosody";

    src = fetchgit {
      url = "https://github.com/xnyhps/luasec.git";
      rev = "71ea0b35ce6b3820a13949047a2ba44a2675e789";
      sha256 = "085sgz3f4q9a8h136j90rf6jkfff77gxwggq2288vx65lgmfvgyq";
    };

    makeFlags = [
      "LUAPATH=$(out)/share/lua/${luaVersion}"
      "LUACPATH=$(out)/lib/lua/${luaVersion}"
    ];

    buildFlags = [ "linux" ];
    buildInputs = [ lua openssl ];
  };

  luaSocket = stdenv.mkDerivation rec {
    name = "luasocket-${version}";
    version = "3.0-rc1";

    src = fetchurl {
      url = "https://github.com/diegonehab/luasocket/archive/"
          + "v${version}.tar.gz";
      sha256 = "0j8jx8bjicvp9khs26xjya8c495wrpb7parxfnabdqa5nnsxjrwb";
    };

    buildInputs = [ lua ];
    installFlags = [ "LUAPREFIX_linux=$(out)" ];
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
      install -m 0644 -vD DBI.lua "$out/share/lua/${luaVersion}/DBI.lua"
      install -vD dbdsqlite3.so "$out/lib/lua/${luaVersion}/dbdsqlite3.so"
    '';
  };

  verse = stdenv.mkDerivation {
    name = "verse";

    src = fetchhg {
      url = "http://code.matthewwild.co.uk/verse";
      tag = "34b878d58948833baf0d3beee1d00631f09fae75";
      sha256 = "17inbyj5yhlssl3w5hssibndgvd6kgyl7jksi4f47d7n2ky1ncxi";
    };

    installPhase = installPlainLua + ''
      ln -s init.lua "$out/share/lua/${luaVersion}/verse.lua"
    '';
  };
in stdenv.mkDerivation {
  name = "xmppoke";

  src = fetchhg {
    url = "https://bitbucket.org/xnyhps/xmppoke";
    tag = "2e394abbff99117177bf7aaa1284f79562abf452";
    sha256 = "1jwfkayvn4b2jpm27s8kqpswm5w602nmawmgdbg44s1glp4p30vg";
  };

  buildInputs = [ makeWrapper ];

  postPatch = ''
    sed -i -r -e 's/(driver_name = ")[^"]*/\1SQLite3/p' poke.lua
  '';

  installPhase = let
    luaPaths = [ "$out" luaSec verse luaSocket luaDbi ];
    luaAbsPaths = [ "${prosody}/lib/prosody/?.lua" ];
    luaAbsCPaths = [ "${prosody}/lib/prosody/?.so" ];

    mkPath = base: "${base}/share/lua/${luaVersion}/?.lua";
    mkCPath = base: "${base}/lib/lua/${luaVersion}/?.so";

    pathString = concatStringsSep ";" (map mkPath luaPaths ++ luaAbsPaths);
    cPathString = concatStringsSep ";" (map mkCPath luaPaths ++ luaAbsCPaths);
  in installPlainLua + ''
    mkdir -p "$out/share/lua/${luaVersion}/net"
    ln -s "${prosody}/lib/prosody/net/server_select.lua" \
      "$out/share/lua/${luaVersion}/net/server.lua"

    mkdir -p "$out/share/xmppoke"
    cp -v schema.sqlite3.sql "$out/share/xmppoke/schema.sql"

    makeWrapper "${lua}/bin/lua $out/share/lua/${luaVersion}/poke.lua" \
      "$out/bin/xmppoke" \
      --set LUA_PATH "'${pathString}'" --set LUA_CPATH "'${cPathString}'"
  '';
}
