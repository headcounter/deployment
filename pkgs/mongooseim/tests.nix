{ buildErlang, fetchFromGitHub, erlangPackages, mongooseim }:

buildErlang rec {
  name = "ejabberd_tests";

  inherit (mongooseim) src version;

  sourceRoot = "${src.name}/test/ejabberd_tests";

  patches = [ ./tests.patch ];

  postBuild = ''
    erlc -o ebin run_common_test.erl
  '';

  erlangDeps = with erlangPackages; [ escalus exml katt mustache proper usec ];

  postInstall = ''
    mkdir -p "$out/etc"
    cp test.config roster.template *.spec "$out/etc/"
    cp -a tests "$out"
  '';
}
