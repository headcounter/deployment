# This module automatically discovers zones from BIND and NSD NixOS
# configurations within the current test network and delegates these
# zones from a fake root zone.
{ config, nodes, pkgs, lib, ... }:

{
  services.bind.enable = true;
  services.bind.cacheNetworks = lib.mkForce [ "any" ];
  services.bind.zones = lib.singleton {
    name = ".";
    file = let
      addDot = zone: zone + lib.optionalString (!lib.hasSuffix "." zone) ".";
      mkNsdZoneNames = zones: map addDot (lib.attrNames zones);
      mkBindZoneNames = zones: map (zone: addDot zone.name) zones;
      getZones = cfg: mkNsdZoneNames cfg.services.nsd.zones
                     ++ mkBindZoneNames cfg.services.bind.zones;

      getZonesForNode = attrs: {
        ip = attrs.config.networking.primaryIPAddress;
        zones = getZones attrs.config;
      };

      notMyself = attrs: attrs.config.networking.hostName
                      != config.networking.hostName;
      otherNodes = lib.filterAttrs (lib.const notMyself) nodes;
      zoneInfo = lib.mapAttrsToList (lib.const getZonesForNode) otherNodes;

    in pkgs.writeText "fake-root.zone" ''
      $TTL 3600
      . IN SOA ns.fakedns. admin.fakedns. ( 1 3h 1h 1w 1d )
      ns.fakedns. IN A ${config.networking.primaryIPAddress}
      . IN NS ns.fakedns.
      ${lib.concatImapStrings (num: { ip, zones }: ''
        ns${toString num}.fakedns. IN A ${ip}
        ${lib.concatMapStrings (zone: ''
        ${zone} IN NS ns${toString num}.fakedns.
        '') zones}
      '') (lib.filter (zi: zi.zones != []) zoneInfo)}
    '';
  };
}
