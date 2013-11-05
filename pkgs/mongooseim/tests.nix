{ buildErlang, fetchgit, escalus, exml, mustache }:

buildErlang {
  name = "ejabberd_tests";
  version = "1.2.2";

  src = fetchgit {
    url = "https://github.com/esl/ejabberd_tests.git";
    rev = "359c0b9287b2132026c9a6704169c5359b43f668";
    sha256 = "1wgqhssd3m4xb1vkwp9rxsvxhvivwdnhad4f52i81h6qvbl86z3v";
  };

  postPatch = ''
    sed -i -e '/exml/s|"2\.0\.0"|"2.0.1"|' rebar.config
  '';

  erlangDeps = [ escalus exml mustache ];

  postInstall = ''
    cp -a tests "$out"
  '';
}
