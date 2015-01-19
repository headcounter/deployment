let
  nodes = { pkgs, ... }: let
    localPkgs = import ../../pkgs {
      inherit pkgs;
    };

    patchedPoke = pkgs.lib.overrideDerivation localPkgs.xmppoke (o: {
      postPatch = (o.postPatch or "") + ''
        sed -i -e '/dbi.Connect/{
          s/"xmppoke", *"xmppoke"/"xmppoke", "root"/
          s/"localhost"/nil/
        }' poke.lua
      '';
    });
  in {
    ultron = { config, lib, ... }: with lib; {
      imports = import ../../modules/module-list.nix ++ [
        ../../xmpp.nix ../../domains.nix
      ];

      headcounter.useSnakeOil = true;
      users.extraUsers.mongoose.extraGroups = [ "keys" ];

      headcounter.vhosts = genAttrs testedVHosts (name: {
        device = "eth1";
      });

      networking.firewall.enable = false;
      virtualisation.vlans = [ 1 ];
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

      networking.firewall.enable = false;
      virtualisation.vlans = [ 1 ];

      networking.localCommands = let
        mkRoute = _: vhost: ''
          ip -4 route add '${vhost.ipv4}' dev eth1
          ip -6 route add '${vhost.ipv6}' dev eth1
        '';
      in ''
        ${concatStrings (mapAttrsToList mkRoute vhosts)}
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
          \i ${patchedPoke}/share/xmppoke/schema.pg.sql
        '';
        authentication = ''
          local all xmppoke trust
        '';
      };

      environment.systemPackages = [ patchedPoke ];
    };
  };

  testedVHosts = [ "headcounter" "aszlig" "noicq" "no_icq" ];

  mkVHostTest = vhost: let
    runner = import <nixpkgs/nixos/tests/make-test.nix>;
  in runner ({ pkgs, ... }@attrs: {
    name = "headcounter-vhost-${vhost}";
    nodes = nodes attrs;
    testScript = { nodes, ... }@testAttrs: with pkgs.lib; let
      inherit (nodes.ultron.config.headcounter) vhosts;
      perVhost = import ./per-vhost.nix (getAttr vhost vhosts);
      vhAttrs = if isFunction perVhost then perVhost testAttrs else perVhost;
    in ''
      startAll;

      $ultron->waitForUnit("mongooseim.service");
      $client->waitForUnit("network.target");
      $client->waitForUnit("postgresql.service");

      ${vhAttrs.testScript}
    '';
  });

in with import <nixpkgs/lib>; args: {
  vhosts = genAttrs testedVHosts (flip mkVHostTest args);
}
