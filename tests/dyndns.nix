let
  mkNetConfig = ipSuffix: { lib, ... }: {
    networking.useDHCP = false;
    networking.interfaces.eth1 = {
      ip4 = lib.singleton {
        address = "192.168.0.${toString ipSuffix}";
        prefixLength = 24;
      };
      ip6 = lib.singleton {
        address = "fc00::${toString ipSuffix}";
        prefixLength = 64;
      };
    };
  };

in import ./make-test.nix ({ lib, ... }: {
  name = "dyndns";

  nodes = {
    nameserver = { config, pkgs, lib, ... }: {
      imports = [ ../common.nix (mkNetConfig 1) ];

      services.nsd = {
        enable = true;

        interfaces = lib.mkForce [];
        xfrdReloadTimeout = 0;
        verbosity = 1;

        zones."example.org.".data = ''
          @ SOA ns.example.org noc.example.org 666 7200 3600 1209600 3600
          ns IN A 192.168.0.1
          ns IN AAAA fc00::1

          alice IN NS ns.example.com
          bob IN NS ns.example.com
        '';

        extraConfig = ''
          # XXX for <nixpkgs> before 8442a7d12c399cc8bbe6cd6c4092b0df9f55dbac
          remote-control:
            control-port: 8952

          pattern:
            name: "dyndns"
            zonefile: "/var/lib/nsd/dynzones/%s.zone"
        '';

        remoteControl = let
          snakeOil = pkgs.runCommand "nsd-control-certs" {
            buildInputs = [ pkgs.openssl ];
          } ''
            mkdir -p "$out"
            "${pkgs.nsd}/bin/nsd-control-setup" -d "$out"
          '';
        in {
          enable = true;
          interfaces = lib.mkForce [ "127.0.0.1" ];
          controlKeyFile = "${snakeOil}/nsd_control.key";
          controlCertFile = "${snakeOil}/nsd_control.pem";
          serverKeyFile = "${snakeOil}/nsd_server.key";
          serverCertFile = "${snakeOil}/nsd_server.pem";
        };
      };

      headcounter.services.dyndns.slave = {
        enable = true;
        master.host = "192.168.0.2";
        zoneCommand = toString (pkgs.writeScript "write-zone" ''
          #!${pkgs.stdenv.shell} -e
          fqdn="$1"
          zonefile="/var/lib/nsd/dynzones/$fqdn.zone"

          # XXX!
          cfgfile="$(systemctl show -p ExecStart nsd.service \
            | sed -re 's/^.* ([^ ]+\.conf).*$/\1/')"

          touchZonefile() {
            touch -r "$zonefile" -d '1 sec' "$zonefile"
          }

          ctrl() {
            "${pkgs.nsd}/bin/nsd-control" -c "$cfgfile" "$@"
          }

          mkdir -p "$(dirname "$zonefile")"
          if [ -e "$zonefile" ]; then
            oldMTime="$(stat -c %Y "$zonefile")"
            exists=1
          else
            oldMTime=0
            exists=0
          fi
          cat > "$zonefile"
          if [ "$oldMTime" -eq "$(stat -c %Y "$zonefile")" ]; then
            touchZonefile
          fi
          coproc waitForUpdate {
            "${pkgs.inotify-tools}/bin/inotifywait" \
              --format %w -m -e close "$zonefile" 2>&1
          }
          watching=0
          while read line <&''${waitForUpdate[0]}; do
            if [ "x$line" = "xWatches established." ]; then
              watching=1
              break
            fi
          done
          if [ $watching -eq 0 ]; then
            kill -TERM %% &> /dev/null || :
            echo "Could not establish inotify watch for $zonefile!" >&2
            exit 1
          fi
          if [ $exists -eq 1 ]; then
            echo -n "Reloading zone $fqdn: " >&2
            ctrl reload "$fqdn" >&2
          else
            ctrl addzone "$fqdn" dyndns
          fi

          for waitTime in 1 2 5 10 30; do
            if read -t $waitTime line <&''${waitForUpdate[0]}; then
              if [ "x$line" = "x$zonefile" ]; then
                kill -TERM %% &> /dev/null || :
                wait &> /dev/null || :
                echo "Reload of $fqdn successful." >&2
                exit 0
              fi
            fi
            echo "Reload of $fqdn failed, touching zone file and" \
                 "resending reload..." >&2
            touchZonefile
            read touched <&''${waitForUpdate[0]}
            ctrl reload "$fqdn" >&2
          done
          echo "Reloading of zone $fqdn failed after 5 retries." >&2
          exit 1
        '');
      };
    };

    webserver = {
      imports = [ ../common.nix (mkNetConfig 2) ];

      headcounter.services.dyndns.master = {
        enable = true;
        emailAddress = "noc@example.org";
        nameservers = [ "ns.example.org" ];
        slave.host = "*";
        credentials = {
          alice.password = "myrealpassword";
          alice.domains = [ "alice.example.org" "alice2.example.org" ];

          bob.password = "anotherrealpassword";
          bob.domains = [ "bob.example.org" ];
        };
      };
    };

    client = { lib, ... }: {
      imports = [ ../common.nix (mkNetConfig 10) ];
      networking.nameservers = lib.mkForce [ "192.168.0.1" "fc00::1" ];
    };
  };

  testScript = let
    mkURL = allAttrs: let
      mkQstring = key: val: "${key}=${val}";
      attrs = removeAttrs allAttrs [ "fail" ];
      qstring = lib.concatStringsSep "&" (lib.mapAttrsToList mkQstring attrs);
    in "http://webserver:3000/?${qstring}";

    dynTest = attrs: let
      method = if attrs.fail or false then "fail" else "succeed";
    in ''
      $client->${method}("curl -f '${mkURL attrs}'");
    '';

  in ''
    my %dnsReplies;

    sub expectDNS {
      my ($type, $fqdn, $expect, $eserial) = @_;
      my $uctype = uc $type;
      my $addr = uc $expect;
      my $soaCmd = "host -r -t soa $fqdn 2> /dev/null | grep serial";
      $client->nest("waiting for $uctype of $fqdn to point to $addr", sub {
        Machine::retry sub {
          my ($soaStatus, $soaOut) = $client->execute($soaCmd);
          return 0 if $soaStatus != 0;
          chomp $soaOut;
          my $serial = $1 if $soaOut =~ /^\s*(\d+)\s*;/ or return 0;
          $client->log("SOA serial is: $serial");
          my ($status, $out) = $client->execute("host -t $type $fqdn");
          return 0 if $status != 0;
          chomp $out;
          my ($rfqdn, $rtype, $raddr) = split /\s+/, $out;
          return 0 if exists $dnsReplies{"$uctype $fqdn"} and
            $dnsReplies{"$uctype $fqdn"} eq "$serial $out";
          return 0 if $serial < $eserial;
          die "expected FQDN $fqdn, but got $rfqdn" if $rfqdn ne $fqdn;
          die "expected type $uctype, but got $rtype" if $rtype ne $uctype;
          die "expected address $addr, but got $raddr" if $raddr ne $addr;
          die "expected SOA serial $eserial, but got $serial"
            if $eserial ne $serial;
          $dnsReplies{"$uctype $fqdn"} = "$serial $out";
          return 1;
        };
      });
    }

    startAll;

    $webserver->waitForUnit("dyndns-master.service");
    $nameserver->waitForUnit("nsd.service");

    $nameserver->waitForUnit("dyndns-slave.service");
    $nameserver->waitUntilSucceeds(
      "netstat -ntpe | grep -q 'ESTABLISHED.*dyndns'"
    );

    $client->waitForUnit("multi-user.target");


    $client->succeed("host ns.example.org");

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
    # dummy
  '';
})
