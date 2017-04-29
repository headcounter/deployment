let
  mkNetConfig = eth: net: ipSuffix: { lib, ... }: {
    networking.useDHCP = lib.mkForce false;
    virtualisation.vlans = lib.mkOrder eth (lib.singleton (net + 1));
    networking.interfaces.${"eth${toString eth}"} = lib.mkForce {
      ip4 = lib.singleton {
        address = "192.168.${toString net}.${toString ipSuffix}";
        prefixLength = 24;
      };
      ip6 = lib.singleton {
        address = "fc0${toString net}::${toString ipSuffix}";
        prefixLength = 64;
      };
    };
  };

  mkSlaveConfig = masterAddress: masterDevice: { lib, ... }: {
    services.nsd = {
      enable = true;
      interfaces = lib.mkForce [];
      verbosity = 1;

      zones."example.org.".data = ''
        @ SOA ns1.example.org noc.example.org 666 7200 3600 1209600 3600
        ns1 IN A 192.168.0.1
        ns1 IN AAAA fc00::1

        ns2 IN A 192.168.1.1
        ns2 IN AAAA fc01::1

        www IN A 192.168.0.2
        www IN AAAA fc00::2

        alice IN NS ns1
        alice IN NS ns2

        alice2 IN NS ns1
        alice2 IN NS ns2

        bob IN NS ns1
        bob IN NS ns2
      '';
    };

    headcounter.services.dyndns.slave = {
      enable = true;
      useNSD = true;
      master.host = masterAddress;
      master.device = masterDevice;
    };
  };

in import ./make-test.nix ({ pkgs, lib, ... }: {
  name = "dyndns";

  nodes = {
    nameserver1 = {
      imports = [
        (mkNetConfig 1 0 1)
        (mkSlaveConfig "192.168.0.2" "eth1")
      ];
    };

    nameserver2 = {
      imports = [
        (mkNetConfig 1 1 1)
        (mkSlaveConfig "192.168.1.2" "eth1")
      ];
    };

    webserver = {
      imports = [ (mkNetConfig 1 0 2) (mkNetConfig 2 1 2) ];

      headcounter.services.dyndns.master = {
        enable = true;
        emailAddress = "noc@example.org";
        nameservers = [ "ns.example.org" ];
        slave = [
          { host = "192.168.0.2";
            device = "eth1";
          }
          { host = "192.168.1.2";
            device = "eth2";
          }
        ];
        credentials = {
          alice.password = "myrealpassword";
          alice.domains = [ "alice.example.org" "alice2.example.org" ];

          bob.password = "anotherrealpassword";
          bob.domains = [ "bob.example.org" ];
        };
      };
    };

    client = { lib, ... }: {
      imports = [ (mkNetConfig 1 0 10) (mkNetConfig 2 1 10) ];
      networking.nameservers = lib.mkForce [
        "192.168.0.1" "fc00::1"
        "192.168.1.1" "fc01::1"
      ];
    };
  };

  testScript = { nodes, ... }: let
    mkURL = allAttrs: let
      mkQstring = key: val: "${key}=${val}";
      attrs = removeAttrs allAttrs [ "fail" ];
      qstring = lib.concatStringsSep "&" (lib.mapAttrsToList mkQstring attrs);
    in "http://www.example.org:3000/?${qstring}";

    dynTest = attrs: let
      method = if attrs.fail or false then "fail" else "succeed";
    in ''
      $client->${method}("curl -f '${mkURL attrs}'");
    '';

    masterConfig = nodes.webserver.config;

  in ''
    my %dnsReplies;

    sub expectDNS {
      my ($type, $fqdn, $expect, $eserial) = @_;
      my $uctype = uc $type;
      my $addr = uc $expect;
      for my $ns ("ns1.example.org", "ns2.example.org") {
        my $soaCmd = "host -r -t soa $fqdn $ns 2> /dev/null | grep serial";
        my $msg = "waiting for $uctype of $fqdn to point to $addr on $ns";
        $client->nest($msg, sub {
          Machine::retry sub {
            my ($soaStatus, $soaOut) = $client->execute($soaCmd);
            return 0 if $soaStatus != 0;
            chomp $soaOut;
            my $serial = $1 if $soaOut =~ /^\s*(\d+)\s*;/ or return 0;
            $client->log("SOA serial is: $serial");
            my ($status, $out) = $client->execute("host -t $type $fqdn $ns");
            return 0 if $status != 0;
            chomp $out;
            my ($rfqdn, $rtype, $raddr) = split /\s+/, $out;
            return 0 if exists $dnsReplies{"$ns $uctype $fqdn"} and
              $dnsReplies{"$ns $uctype $fqdn"} eq "$serial $out";
            return 0 if $serial < $eserial;
            die "expected FQDN $fqdn, but got $rfqdn" if $rfqdn ne $fqdn;
            die "expected type $uctype, but got $rtype" if $rtype ne $uctype;
            die "expected address $addr, but got $raddr" if $raddr ne $addr;
            die "expected SOA serial $eserial, but got $serial"
              if $eserial ne $serial;
            $dnsReplies{"$ns $uctype $fqdn"} = "$serial $out";
            return 1;
          };
        });
      }
    }

    startAll;

    $webserver->waitForUnit("dyndns-master.service");

    for my $ns ($nameserver1, $nameserver2) {
      $ns->waitForUnit("nsd.service");
      $ns->waitForUnit("dyndns-slave.service");
      $ns->waitUntilSucceeds("netstat -ntpe | grep -q 'ESTABLISHED.*dyndns'");
    }

    $client->waitForUnit("multi-user.target");

    $client->succeed("host ns1.example.org ns1.example.org");
    $client->succeed("host ns2.example.org ns2.example.org");

    ${dynTest {
      username = "alice";
      password = "myrealpassword";
      domain = "alice.example.org";
      ipaddr = "1.2.3.4";
      ip6addr = "1:2:3::4";
    }}

    expectDNS("a", "alice.example.org", "1.2.3.4", 1);
    expectDNS("aaaa", "alice.example.org", "1:2:3:0:0:0:0:4", 1);

    ${dynTest {
      fail = true;
      username = "bob";
      password = "anotherrealpassword";
      domain = "alice.example.org";
      ipaddr = "4.3.2.1";
      ip6addr = "4:3:2::1";
    }}

    ${dynTest {
      fail = true;
      username = "alice";
      password = "invalidpassword";
      domain = "alice.example.org";
      ipaddr = "4.3.2.1";
      ip6addr = "4:3:2::1";
    }}

    ${dynTest {
      username = "bob";
      password = "anotherrealpassword";
      domain = "bob.example.org";
      ipaddr = "9.9.9.9";
      ip6addr = "abcd::ef";
    }}

    expectDNS("a", "bob.example.org", "9.9.9.9", 1);
    expectDNS("aaaa", "bob.example.org", "abcd:0:0:0:0:0:0:ef", 1);

    ${dynTest {
      username = "alice";
      password = "myrealpassword";
      domain = "alice.example.org";
      ip6addr = "12:34::56";
    }}

    expectDNS("aaaa", "alice.example.org", "12:34:0:0:0:0:0:56", 2);
    expectDNS("a", "alice.example.org", "1.2.3.4", 2);

    ${dynTest {
      username = "alice";
      password = "myrealpassword";
      domain = "alice2.example.org";
      ipaddr = "9.8.7.6";
    }}

    expectDNS("a", "alice2.example.org", "9.8.7.6", 1);
    $client->fail("host -t aaaa alice2.example.org");

    $client->succeed(
      'for i in $(seq 20); do'.
      ' hexnum="$(printf "%x" "$i")";'.
      ' curl -f "${mkURL {
        username = "bob";
        password = "anotherrealpassword";
        domain = "bob.example.org";
        ip6addr = "666::$hexnum";
      }}";'.
      ' done'
    );

    ${dynTest {
      username = "bob";
      password = "anotherrealpassword";
      domain = "bob.example.org";
      ipaddr = "2.2.2.2";
    }}

    expectDNS("a", "bob.example.org", "2.2.2.2", 22);
    expectDNS("aaaa", "bob.example.org", "666:0:0:0:0:0:0:14", 22);

    $nameserver1->succeed('systemctl restart dyndns-slave.service');
    $nameserver2->succeed('systemctl restart dyndns-slave.service');

    ${dynTest {
      username = "bob";
      password = "anotherrealpassword";
      domain = "bob.example.org";
      ipaddr = "3.3.3.3";
    }}

    expectDNS("a", "bob.example.org", "3.3.3.3", 23);
  '';
})
