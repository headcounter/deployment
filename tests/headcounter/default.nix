let
  nodes = { pkgs, ... }: let
    patchedPoke = pkgs.lib.overrideDerivation pkgs.headcounter.xmppoke (o: {
      postPatch = (o.postPatch or "") + ''
        sed -ri -e 's/(db_host *= *)[^,]*/\1nil/' \
                -e '/Connecting to database/d' \
                poke.lua
      '';
    });
  in {
    ultron = { config, lib, ... }: with lib; {
      imports = [ ../../xmpp.nix ../../domains.nix ];

      headcounter.useSnakeOil = true;
      users.extraUsers.mongoose.extraGroups = [ "keys" ];

      headcounter.vhostDefaultDevice = "eth1";

      virtualisation.vlans = [ 1 ];
      virtualisation.memorySize = 2048;
    };

    client = { nodes, pkgs, config, lib, ... }: with lib; let
      inherit (nodes.ultron.config.headcounter) vhosts;
    in {
      networking.extraHosts = let
        mkHostEntry = _: vhost: lib.optionalString (vhost.fqdn != null) ''
          ${vhost.ipv4} ${vhost.fqdn}
          ${vhost.ipv6} ${vhost.fqdn}
        '';
      in concatStrings (mapAttrsToList mkHostEntry vhosts);

      virtualisation.vlans = [ 1 ];

      networking.localCommands = ''
        ${concatStrings (mapAttrsToList (const (vhost: ''
          ip -4 route add '${vhost.ipv4}' dev eth1
          ip -6 route add '${vhost.ipv6}' dev eth1
        '')) vhosts)}
        ip -4 route flush cache
        ip -6 route flush cache
      '';

      services.postgresql = {
        enable = true;
        package = pkgs.postgresql;
        initialScript = pkgs.writeText "init.sql" ''
          CREATE ROLE xmppoke WITH LOGIN;
          CREATE DATABASE xmppoke OWNER xmppoke;
          \c xmppoke
          BEGIN;
          \i ${patchedPoke}/share/xmppoke/schema.pg.sql
          COMMIT;
        '';
        authentication = ''
          local all xmppoke trust
        '';
      };

      environment.systemPackages = [
        patchedPoke pkgs.headcounter.xmppokeReport
      ];
    };
  };

  testedVHosts = [ "headcounter" "aszlig" "noicq" "no_icq" "torservers" ];

  mkVHostTest = vhost: import ../make-test.nix ({ pkgs, lib, ... }@attrs: let
    inherit (import ../mongooseim/lib.nix {
      inherit pkgs lib;
    }) runInCtl checkListeners;
  in {
    name = "headcounter-vhost-${vhost}";
    nodes = nodes attrs;
    testScript = { nodes, ... }@testAttrs: with lib; let
      inherit (nodes.ultron.config.headcounter) vhosts;
      perVhost = import ./per-vhost/xmppoke.nix (getAttr vhost vhosts);
      vhAttrs = if isFunction perVhost then perVhost testAttrs else perVhost;
    in ''
      my $out = $ENV{'out'};
      startAll;

      $ultron->waitForUnit("mongooseim.service");
      $client->waitForUnit("network.target");
      $client->waitForUnit("postgresql.service");

      ${runInCtl "ultron" (checkListeners nodes.ultron)}

      ${vhAttrs.testScript}
    '';
  });

in with import <nixpkgs/lib>; args: {
  vhosts = genAttrs testedVHosts (flip mkVHostTest args);
}
