import <nixpkgs/nixos/tests/make-test.nix> ({ pkgs, ... }:

let
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

  testedDomains = [ "headcounter" "aszlig" "noicq" "no_icq" ];

in {
  name = "headcounter";

  nodes = {
    ultron = { config, lib, ... }: with lib; {
      imports = import ../../modules/module-list.nix ++ [
        ../../xmpp.nix ../../domains.nix
      ];

      headcounter.useSnakeOil = true;
      users.extraUsers.mongoose.extraGroups = [ "keys" ];

      headcounter.vhosts = genAttrs testedDomains (name: {
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
    };
  };

  testScript = { nodes, ... }: with pkgs.lib; let
    inherit (nodes.ultron.config.headcounter) vhosts;

    vhostTest = vhost: ''
      $client->nest("check availability", sub {
        $client->succeed("ping -c1 ${vhost.fqdn} >&2");
        $client->succeed("nc -z ${vhost.fqdn} 5222");
      });
      $client->succeed("${patchedPoke}/bin/xmppoke ${vhost.fqdn} >&2");
    '';

    mkVhostTest = name: vhost: optionalString (elem name testedDomains) ''
      subtest "vhost-${name}", sub {
        ${vhostTest vhost}
      };
    '';

  in ''
    startAll;
    $ultron->waitForUnit("mongooseim.service");
    $client->waitForUnit("network.target");
    $client->waitForUnit("postgresql.service");

    ${concatStrings (mapAttrsToList mkVhostTest vhosts)}
  '';
})
