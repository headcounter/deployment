{ config, lib, nodes, ... }:

{
  deployment.hetzner.partitions = ''
    clearpart --all --initlabel --drives=vda
    part swap --size=2000 --label=swap --fstype=swap --ondisk=vda
    part / --fstype=ext4 --label=root --grow --ondisk=vda
  '';

  headcounter.services.dyndns.slave = {
    enable = true;
    useNSD = true;
    master = let
      myself = config.networking.hostName;
      tunnel = nodes.ultron.config.networking.p2pTunnels.ssh.${myself};
    in lib.singleton {
      host = tunnel.remoteIPv4;
      device = "tun${toString tunnel.remoteTunnel}";
    };
  };

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

    # FIXME: torservers.net needs to be handled differently here!
    mkZone = domain: text: ''
      $ORIGIN ${domain}.
      $TTL 60
      ${mkSOA "postmaster.${domain}"}
      @ IN NS ${primaryDNS}.
      @ IN NS ${secondaryDNS}.
      @ IN MX 10 mailfresser.de.

      _acme-challenge IN NS ${primaryDNS}.

      ${text}
    '';

    mkXMPPRecords = dest: ''
      conference  IN  CNAME ${dest}
      irc         IN  CNAME ${dest}
      pubsub      IN  CNAME ${dest}
      vjud        IN  CNAME ${dest}
      icq         IN  CNAME ${dest}
      proxy       IN  CNAME ${dest}

      _jabber._tcp      IN SRV 10 0 5269 ${dest}
      _xmpp-server._tcp IN SRV 10 0 5269 ${dest}
      _xmpp-client._tcp IN SRV 10 0 5222 ${dest}
    '';

    # XXX: Don't hardcode ultron here!
    inherit (nodes.ultron.config.headcounter) vhosts;

  in {
    enable = true;
    interfaces = lib.mkForce []; # all interfaces
    zones = {
      "aszlig.net.".data = mkZone "aszlig.net" ''
        ; Generic stuff
        @      IN  A     ${vhosts.aszlig.ipv4}
        @      IN  AAAA  ${vhosts.aszlig.ipv6}

        ; Legacy DNS servers
        ns1 IN A 85.10.206.212
        ns2 IN A 217.20.112.88

        ${mkXMPPRecords "@"}
      '';

      "headcounter.org.".data = mkZone "headcounter.org" ''
        ; Generic stuff
        @           IN  A     ${vhosts.headcounter.ipv4}
        @           IN  AAAA  ${vhosts.headcounter.ipv6}

        ; Hosts of the new deployment
        taalo      IN A    188.40.96.202
        taalo      IN AAAA 2a01:4f8:100:726f::
        benteflork IN A    144.76.202.147
        benteflork IN AAAA 2a01:4f8:200:8392::
        ultron     IN A    5.9.105.142
        ultron     IN AAAA 2a01:4f8:162:4187::

        ; DNS servers
        ns1 IN A    78.46.182.124
        ns1 IN AAAA 2a01:4f8:d13:3009::2
        ns2 IN A    78.47.142.38
        ns2 IN AAAA 2a01:4f8:d13:5308::2

        ${lib.concatMapStringsSep "\n" (name: ''
        ${name} IN NS ${primaryDNS}.
        ${name} IN NS ${secondaryDNS}.
        '') [ "zrnzrk" "docsnyder" ]}

        ${mkXMPPRecords "@"}
      '';

      "no-icq.org.".data = mkZone "no-icq.org" ''
        ; Generic stuff
        @      IN  A    ${vhosts.no_icq.ipv4}
        @      IN  AAAA ${vhosts.no_icq.ipv6}

        ${mkXMPPRecords "@"}
      '';

      "noicq.org.".data = mkZone "noicq.org" ''
        ; Generic stuff
        @      IN  A    ${vhosts.noicq.ipv4}
        @      IN  AAAA ${vhosts.noicq.ipv6}

        ${mkXMPPRecords "@"}
      '';

      "jabber.torservers.net.".data = mkZone "jabber.torservers.net" ''
        ; Generic stuff
        @      IN  A    ${vhosts.torservers.ipv4}
        @      IN  AAAA ${vhosts.torservers.ipv6}

        ${mkXMPPRecords "@"}
      '';
    };
  };
}
