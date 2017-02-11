{ lib, buildErlang, fetchFromGitHub, erlangPackages, mongooseim }:

buildErlang rec {
  name = "ejabberd_tests";

  inherit (mongooseim) src version;

  sourceRoot = "${src.name}/test/ejabberd_tests";

  patches = [
    ./ctl-path.patch ./dont-tamper-with-config.patch ./no-fed-node.patch
  ];

  postPatch = ''
    # Remove tests that require changing cluster settings or modifying
    # configuration files.
    ${lib.concatMapStrings (suite: ''
      sed -i -e '/^ *{ *suites *,[^,]*, *'${
        lib.escapeShellArg "${suite}_SUITE"
      }' *} *\. *$/d' *.spec
    '') [
      "cluster_commands" "conf-reload" "connect" "metrics_api" "users_api"
    ]}

    # Increase timeouts for a few escalus wait_for_stanza calls:
    sed -i -e '/wait_for_stanza/s/10000/&0/' tests/s2s_SUITE.erl
    sed -i -e '/wait_for_stanza/s/5000/&0/' tests/amp_SUITE.erl
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
