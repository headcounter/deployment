{ lib, ... }@passthru:

let
  # This is for all machines in the deployment, because when run on Hydra, all
  # of the deployment tests are run at once which causes a lot of services to
  # time out because of the high load.
  timeoutConfig = { lib, ... }: {
    systemd.extraConfig = ''
      DefaultTimeoutStartSec=600s
      DefaultTimeoutStopSec=600s
    '';
    systemd.services.postgresql.serviceConfig.TimeoutSec = lib.mkForce 600;
  };

  # All the machines in the deployment
  deployment = lib.mapAttrs (node: config: {
    imports = [ config ../../modules/testing/nixops.nix timeoutConfig ];

    config = {
      headcounter.nixops = {
        inherit (import ../../network.nix) resources;
      };
    } // lib.optionalAttrs (node == "ultron") {
      headcounter.useSnakeOil = true;
      headcounter.vhostDefaultDevice = "eth1";

      virtualisation.vlans = lib.singleton 1;
      virtualisation.memorySize = 2048;
    };
  }) (builtins.removeAttrs (import ../../network.nix) [
    "network" "defaults" "resources" "require" "_file"
  ]);

  isTestNode = node: !lib.elem node (lib.attrNames deployment);

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

  makeHeadcounterTest = maybeExpr: import ../make-test.nix ({ lib, ... }: let
    isDirect = builtins.isFunction maybeExpr || builtins.isAttrs maybeExpr;
    expr = if isDirect then maybeExpr else import maybeExpr;
    attrs = if builtins.isFunction expr then expr passthru else expr;
  in {
    name = "headcounter-${attrs.name}";

    nodes = lib.zipAttrsWith (node: cfgs: {
      imports = cfgs ++ lib.optional (isTestNode node) testNodeConfig;
    }) [ deployment (attrs.nodes or {}) ];

    testScript = scriptAttrs: let
      subTestScript = if builtins.isFunction attrs.testScript
                      then attrs.testScript scriptAttrs
                      else attrs.testScript;

      runForDeplMachines = command: let
        runCommand = machine: "\$${machine}->${command};";
        machines = lib.attrNames deployment;
      in lib.concatMapStrings runCommand machines;
    in ''
      my $out = $ENV{'out'};

      ${runForDeplMachines "start"}
      ${runForDeplMachines "waitForUnit('multi-user.target')"}

      startAll;

      ${subTestScript}
    '';
  } // removeAttrs attrs [ "name" "nodes" "testScript" ]) passthru;

  testedVHosts = [ "headcounter" "aszlig" "noicq" "no_icq" "torservers" ];

in {
  vhosts = lib.genAttrs testedVHosts (vhost: {
    xmppoke = makeHeadcounterTest (import ./per-vhost/xmppoke.nix vhost);
    escalus = makeHeadcounterTest (import ./per-vhost/escalus vhost);
  });

  listeners = makeHeadcounterTest ./listeners.nix;
}
