{ mkDerivation, async, attoparsec, base, bytestring, cereal, iproute, lens
, network, stdenv, safecopy, stm, template-haskell, text, transformers, unix
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
    attoparsec async base bytestring cereal iproute lens network
    safecopy stm systemd template-haskell text
  ];
  testHaskellDepends = [
    async base bytestring cereal network safecopy stm unix
  ];
  license = stdenv.lib.licenses.agpl3;
}
