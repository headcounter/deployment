# This module automatically discovers zones from BIND and NSD NixOS
# configurations within the current test network and delegates these
# zones from a fake root zone.
{ config, nodes, pkgs, lib, ... }:

{
  services.bind.enable = true;
  services.bind.cacheNetworks = lib.mkForce [ "any" ];
  services.bind.forwarders = lib.mkForce [];
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
        zones = lib.filter (zone: zone != ".") (getZones attrs.config);
      };

      zoneInfo = lib.mapAttrsToList (lib.const getZonesForNode) nodes;

      # All of the zones that are subdomains of existing zones.
      # For example if there is only "example.com" the following zones would be
      # 'subZones':
      #
      #  * foo.example.com.
      #  * bar.example.com.
      #
      # While the following would *not* be 'subZones':
      #
      #  * example.com.
      #  * com.
      #
      subZones = let
        allZones = lib.concatMap (zi: zi.zones) zoneInfo;
        isSubZoneOf = z1: z2: lib.hasSuffix z2 z1 && z1 != z2;
      in lib.filter (z: lib.any (isSubZoneOf z) allZones) allZones;

      # All the zones without 'subZones'.
      filteredZoneInfo = map (zi: zi // {
        zones = lib.filter (x: !lib.elem x subZones) zi.zones;
      }) zoneInfo;

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
      '') (lib.filter (zi: zi.zones != []) filteredZoneInfo)}
    '';
  };
}
