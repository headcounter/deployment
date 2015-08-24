{ buildErlang, fetchFromGitHub, erlangPackages }:

buildErlang rec {
  name = "ejabberd_tests";
  version = "2.4.1";

  src = fetchFromGitHub {
    owner = "esl";
    repo = "ejabberd_tests";
    rev = version;
    sha256 = "08ycfgy9j8jgk79kcb9ryvq704ngd9aj2lf4wcyfsniffc0zgf1l";
  };

  postBuild = ''
    erlc -o ebin run_common_test.erl
  '';

  erlangDeps = with erlangPackages; [ escalus exml mustache katt proper ];

  postInstall = ''
    mkdir -p "$out/etc"
    cp test.config vcard.config *.spec "$out/etc/"
    cp -a tests "$out"
  '';
}
