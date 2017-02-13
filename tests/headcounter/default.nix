{ lib, ... }@passthru:

let
  # Machine config for the Headcounter XMPP server (only XMPP specific parts).
  ultron = { config, lib, ... }: {
    imports = [ ../../xmpp.nix ../../domains.nix ];

    headcounter.useSnakeOil = true;
    users.extraUsers.mongoose.extraGroups = lib.singleton "keys";

    headcounter.vhostDefaultDevice = "eth1";

    virtualisation.vlans = lib.singleton 1;
    virtualisation.memorySize = 2048;
  };

  # Common config for each test node that's not ultron.
  testNodeConfig = { lib, nodes, ... }: let
    inherit (nodes.ultron.config.headcounter) vhosts;
  in {
    # We don't have DNS (yet), so let's add the nodes to /etc/hosts.
    networking.extraHosts = let
      mkHostEntry = vhost: lib.optionalString (vhost.fqdn != null) ''
        ${vhost.ipv4} ${vhost.fqdn}
        ${vhost.ipv6} ${vhost.fqdn}
      '';
    in lib.concatStrings (lib.mapAttrsToList (lib.const mkHostEntry) vhosts);

    virtualisation.vlans = [ 1 ];

    # Add the subnets for the various virtual interfaces of ultron
    networking.localCommands = ''
      ${lib.concatStrings (lib.mapAttrsToList (lib.const (vhost: ''
        ip -4 route add '${vhost.ipv4}' dev eth1
        ip -6 route add '${vhost.ipv6}' dev eth1
      '')) vhosts)}
      ip -4 route flush cache
      ip -6 route flush cache
    '';
  };

  makeHeadcounterTest = expr: import ../make-test.nix ({ lib, ... }: let
    attrs = if builtins.isFunction expr then expr passthru else expr;
  in {
    name = "headcounter-${attrs.name}";

    nodes = { inherit ultron; } // lib.mapAttrs (node: cfg: {
      imports = [
        (if node == "ultron" then ultron else testNodeConfig) cfg
      ];
    }) (attrs.nodes or {});

    testScript = scriptAttrs: let
      subTestScript = if builtins.isFunction attrs.testScript
                      then attrs.testScript scriptAttrs
                      else attrs.testScript;
    in ''
      my $out = $ENV{'out'};
      startAll;

      $ultron->waitForUnit("mongooseim.service");

      ${subTestScript}
    '';
  } // removeAttrs attrs [ "name" "nodes" "testScript" ]) passthru;

  testedVHosts = [ "headcounter" "aszlig" "noicq" "no_icq" "torservers" ];

in {
  vhosts = lib.genAttrs testedVHosts (vhost: {
    xmppoke = makeHeadcounterTest (import ./per-vhost/xmppoke.nix vhost);
  });

  listeners = makeHeadcounterTest ({ pkgs, lib, ... }: let
    inherit (import ../mongooseim/lib.nix {
      inherit pkgs lib;
    }) runInCtl checkListeners;
  in {
    name = "listeners";
    testScript = { nodes, ... }:
      runInCtl "ultron" (checkListeners nodes.ultron);
  });
}
