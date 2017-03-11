vhost:

{ pkgs, lib, ... }: let

  testLibs = let
    pkg = pkgs.headcounter.mongooseimTests;
    deps = lib.singleton pkg ++ pkg.recursiveErlangDeps;
  in map (d: "${d.appDir}/ebin") deps;

  users = {
    outsider.password = "truly secure, no?";
    outsider.shouldExist = false;

    admin.host = "aszlig.net";
    admin.password = "big admin";

    wallop.host = "aszlig.net";
    wallop.password = "small wallop";

    toradmin.host = "torservers.net";
    toradmin.password = "torservers dedicated admin";
  };

  mkTestConfig = hclib: fqdn: let
    mkSpecVal = val: if lib.isString val then { binary = val; } else val;
    mkUser = name: spec: {
      username.binary = name;
      server.binary = spec.host or fqdn;
      host.binary = spec.server or fqdn;
      starttls.atom = "required";
      auth_method.binary = "SCRAM-SHA-1";
    } // lib.mapAttrs (lib.const mkSpecVal) spec;
  in pkgs.writeText "test.config" ''
    {escalus_xmpp_server, escalus_mongooseim}.
    {escalus_host, <<"${fqdn}">>}.

    {escalus_users, ${hclib.erlPropList (lib.mapAttrs mkUser users)}}.
  '';

  registerUser = fqdn: name: spec: let
    user = spec.username or name;
    host = spec.host or fqdn;
    inherit (spec) password;
    shouldExist = spec.shouldExist or true;
    cmd = [ "mongooseimctl" "register" name host password ];
    shellCmd = lib.concatMapStringsSep " " lib.escapeShellArg cmd;
    perlCmd = "$ultron->succeed('${lib.escape ["'"] shellCmd}');";
  in lib.optionalString shouldExist perlCmd;

  testRunner = [
    "${pkgs.erlang}/bin/ct_run"
    "-noinput" "-noshell"
    "-config" "/etc/headcounter/test.config"
    "-ct_hooks" "ct_tty_hook" "[]"
    "-logdir" "ct_report"
    "-dir" "test"
    "-erl_args"
    "-pa"
  ] ++ testLibs;

  sourceTree = pkgs.runCommand "test-source-tree" {} ''
    mkdir -p "$out/test"
    cp "${./headcounter_SUITE.erl}" "$out/test/headcounter_SUITE.erl"
  '';

in {
  name = "vhost-${vhost}-escalus";

  nodes.client = { nodes, hclib, ... }: let
    inherit (nodes.ultron.config.headcounter.vhosts.${vhost}) fqdn;
  in {
    virtualisation.memorySize = 1024;
    environment.etc."headcounter/test.config".source = mkTestConfig hclib fqdn;
  };

  excludeNodes = [ "taalo" "benteflork" "unzervalt" ];

  testScript = { nodes, ... }: let
    inherit (nodes.ultron.config.headcounter.vhosts.${vhost}) fqdn;
  in ''
    ${lib.concatStrings (lib.mapAttrsToList (registerUser fqdn) users)}

    $client->succeed('cp -Lr ${sourceTree}/* .');

    ${(import ../../../mongooseim/lib.nix {
      inherit pkgs lib;
    }).runCommonTests testRunner}
  '';
}
