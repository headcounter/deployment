{ buildErlang, fetchgit }:

buildErlang rec {
  name = "reloader";
  version = "1";

  src = fetchgit {
    url = "https://github.com/lucaspiller/${name}.git";
    rev = "9dd05d613c2abe563bc1c472950b96d2a832663b";
    sha256 = "0jdg3sp005n6jjh7di4xcikmjzzd80lnkr64k3x3a3xzd5vmmqxh";
  };
}
