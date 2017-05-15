{ mkDerivation, async, attoparsec, base, bytestring, cereal
, iproute, lens, network, process, safecopy, stdenv, stm, systemd
, template-haskell, text, unix
}:
mkDerivation {
  pname = "nexus";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    async attoparsec base bytestring cereal iproute lens network
    process safecopy stm systemd template-haskell text
  ];
  testHaskellDepends = [
    async base bytestring cereal network stm unix
  ];
  license = stdenv.lib.licenses.agpl3;
}
