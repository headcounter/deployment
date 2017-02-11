{ buildErlang, fetchFromGitHub, erlangPackages, mongooseim }:

buildErlang rec {
  name = "ejabberd_tests";

  inherit (mongooseim) src version;

  sourceRoot = "${src.name}/test/ejabberd_tests";

  patches = [
    ./ctl-path.patch ./dont-tamper-with-config.patch ./no-fed-node.patch
  ];

  postPatch = ''
    # Remove tests for metrics_api
    sed -i -e '/^ *{ *suites *,[^,]*, *metrics_api_SUITE *} *\. *$/d' *.spec
  '';

  postBuild = ''
    erlc -o ebin run_common_test.erl
  '';

  erlangDeps = with erlangPackages; [
    cowboy erlsh escalus exml jiffy katt mustache proper shotgun usec
  ];

  postInstall = ''
    mkdir -p "$out/etc"
    cp test.config roster.template *.spec "$out/etc/"
    cp -a tests "$out"
  '';
}
