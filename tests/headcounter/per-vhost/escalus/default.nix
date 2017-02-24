vhost:

{ pkgs, lib, ... }: let

  testLibs = let
    pkg = pkgs.headcounter.mongooseimTests;
    deps = lib.singleton pkg ++ pkg.recursiveErlangDeps;
  in map (d: "${d.appDir}/ebin") deps;

  users = {
    outsider.password = "truly secure, no?";
  };

  mkTestConfig = hclib: fqdn: let
    mkSpecVal = val: if lib.isString val then { binary = val; } else val;
    mkUser = name: spec: {
      username.binary = name;
      server.binary = fqdn;
      host.binary = fqdn;
      starttls.atom = "required";
    } // lib.mapAttrs (lib.const mkSpecVal) spec;
  in pkgs.writeText "test.config" ''
    {escalus_xmpp_server, escalus_mongooseim}.
    {escalus_host, <<"${fqdn}">>}.

    {escalus_users, ${hclib.erlPropList (lib.mapAttrs mkUser users)}}.
  '';

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
    cp ${./headcounter_SUITE.erl} "$out/test/headcounter_SUITE.erl"
  '';

in {
  name = "vhost-${vhost}-escalus";

  nodes.client = { nodes, hclib, ... }: let
    inherit (nodes.ultron.config.headcounter.vhosts.${vhost}) fqdn;
  in {
    environment.etc."headcounter/test.config".source = mkTestConfig hclib fqdn;
  };

  testScript = ''
    $client->succeed('cp -Lr ${sourceTree}/* .');

    ${(import ../../../mongooseim/lib.nix {
      inherit pkgs lib;
    }).runCommonTests testRunner}
  '';
}
