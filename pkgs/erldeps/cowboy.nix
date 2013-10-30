{ buildErlang, fetchurl, ranch }:

buildErlang rec {
  name = "cowboy";
  version = "0.8.3";

  src = fetchurl {
    url = "https://github.com/extend/cowboy/archive/${version}.tar.gz";
    sha256 = "0b5y99blimfbmqxvf6jlgq3f7rl7igb7b8msc65ivskyv297i479";
  };

  erlangDeps = [ ranch ];
}
