{ buildErlang, fetchFromGitHub, erlangPackages }:

buildErlang rec {
  name = "ejabberd_tests";
  version = "2.4.0";

  src = fetchFromGitHub {
    owner = "esl";
    repo = "ejabberd_tests";
    rev = version;
    sha256 = "0k9004hnm9ir2la2lg681l897l20yv8k5fmzhjq3krk78cbnj1dj";
  };

  postBuild = ''
    erlc -Ideps/exml/include -o ebin run_common_test.erl
  '';

  erlangDeps = with erlangPackages; [ escalus exml mustache katt proper ];

  postInstall = ''
    ensureDir "$out/etc"
    cp test.config vcard.config *.spec "$out/etc/"
    cp -a tests deps "$out"
  '';
}
