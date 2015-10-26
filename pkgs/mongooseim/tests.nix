{ buildErlang, fetchFromGitHub, erlangPackages }:

buildErlang rec {
  name = "ejabberd_tests";
  version = "2.4.1";

  src = fetchFromGitHub {
    owner = "esl";
    repo = "ejabberd_tests";
    rev = "4429b1ea9646836e3b3e26d5ef085a47773a456a";
    sha256 = "0cwfwp6if5hdgirp66m0h281319nvvxiw70kv4fyn945aj0al2r3";
  };

  patches = [ ./tests.patch ];

  postBuild = ''
    erlc -o ebin run_common_test.erl
  '';

  erlangDeps = with erlangPackages; [ escalus exml katt mustache proper usec ];

  postInstall = ''
    mkdir -p "$out/etc"
    cp test.config vcard.config *.spec "$out/etc/"
    cp -a tests "$out"
  '';
}
