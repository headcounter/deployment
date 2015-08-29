{ lib, ... }:

{
  deployment.hetzner.partitions = ''
    clearpart --all --initlabel --drives=vda
    part swap --size=2000 --label=swap --fstype=swap --ondisk=vda
    part / --fstype=ext4 --label=root --grow --ondisk=vda
  '';

  services.nsd = let
    primaryDNS   = "ns1.headcounter.org";
    secondaryDNS = "ns2.headcounter.org";

    mkSOA = dottedEmail: ''
      @ IN SOA ${primaryDNS}. ${dottedEmail}. (
        0       ; Serial
        28800   ; Refresh
        7200    ; Retry
        604800  ; Expire
        86400   ; Negative Cache TTL
      )
    '';

    mkZone = domain: text: ''
      \$ORIGIN ${domain}.
      \$TTL 60
      ${mkSOA "postmaster.${domain}"}
      @ IN NS ${primaryDNS}.
      @ IN NS ${secondaryDNS}.
      @ IN MX 10 mailfresser.de.

      ${text}
    '';

    mkXMPPRecords = dest: ''
      conference  IN  CNAME ${dest}
      irc         IN  CNAME ${dest}
      pubsub      IN  CNAME ${dest}
      vjud        IN  CNAME ${dest}
      icq         IN  CNAME ${dest}
      aim         IN  CNAME ${dest}
      msn         IN  CNAME ${dest}
      yahoo       IN  CNAME ${dest}
      mail        IN  CNAME ${dest}
      jmc         IN  CNAME ${dest}
      gadugadu    IN  CNAME ${dest}
      sms         IN  CNAME ${dest}
      proxy       IN  CNAME ${dest}

      _jabber._tcp      IN SRV 10 0 5269 ${dest}
      _xmpp-server._tcp IN SRV 10 0 5269 ${dest}
      _xmpp-client._tcp IN SRV 10 0 5222 ${dest}
    '';

  in {
    enable = true;
    interfaces = lib.mkForce []; # all interfaces
    zones = {
      "aszlig.net.".data = mkZone "aszlig.net" ''
        ; Generic stuff
        @      IN  A     88.198.198.218
        @      IN  AAAA  2001:6f8:900:72a::2
        jabber IN  A     88.198.43.4

        ; Legacy DNS servers
        ns1 IN A 85.10.206.212
        ns2 IN A 217.20.112.88

        ; @ is legacy server
        ${mkXMPPRecords "@"}
      '';

      "headcounter.org.".data = mkZone "headcounter.org" ''
        ; Hosts of the new deployment
        taalo      IN A 144.76.61.117
        benteflork IN A 144.76.202.147
        ultron     IN A 5.9.105.142

        ; DNS servers
        ns1 IN A    78.46.182.124
        ns1 IN AAAA 2a01:4f8:d13:3009::2
        ns2 IN A    78.47.142.38
        ns2 IN AAAA 2a01:4f8:d13:5308::2

        ; Legacy transition stuff
        @           IN  A     78.47.32.129
        @           IN  AAAA  2a01:4f8:162:4187::1
        merkelig    IN  A     88.198.198.219
        merkelig    IN  AAAA  2001:6f8:900:72a::2
        jabber      IN  A     88.198.43.4
        jabber      IN  AAAA  2001:6f8:900:72a::2
        www         IN  A     88.198.43.4
        www         IN  AAAA  2001:6f8:900:72a::2

        ; "merkelig" is legacy server
        ${mkXMPPRecords "merkelig"}
      '';

      "no-icq.org.".data = mkZone "no-icq.org" ''
        ; Generic stuff
        @      IN  A    88.198.198.220
        @      IN  AAAA 2001:6f8:900:72a::2
        jabber IN  A    88.198.43.4
        www    IN  A    88.198.43.4

        ; @ is legacy server
        ${mkXMPPRecords "@"}
      '';

      "noicq.org.".data = mkZone "noicq.org" ''
        ; Generic stuff
        @      IN A    88.198.198.221
        @      IN AAAA 2001:6f8:900:72a::2
        jabber IN A    88.198.43.4
        www    IN A    88.198.43.4

        ; @ is legacy server
        ${mkXMPPRecords "@"}
      '';
    };
  };
}
