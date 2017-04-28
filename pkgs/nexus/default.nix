{ mkDerivation, async, base, bytestring, cereal, network, stdenv
, stm, template-haskell, transformers, unix
}:

let
  systemd = mkDerivation {
    pname = "systemd";
    version = "1.1.2";
    sha256 = "11wjsfnnsfgrffsxy9s5yqlzb7zxlrjg92mhanq66jvbnqh1jijr";
    libraryHaskellDepends = [ base bytestring network transformers unix ];
    doCheck = false;
    homepage = "https://github.com/erebe/systemd";
    description = "Systemd facilities (Socket activation, Notify)";
    license = stdenv.lib.licenses.bsd3;
  };

in mkDerivation {
  pname = "nexus";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    async base bytestring cereal network stm systemd template-haskell
  ];
  testHaskellDepends = [ async base bytestring cereal network stm unix ];
  license = stdenv.lib.licenses.agpl3;
}
