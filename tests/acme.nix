import ./make-test.nix {
  name = "letsencrypt";

  nodes = let
    common = { nodes, lib, ... }: {
      nixpkgs.config.packageOverrides = super: {
        cacert = super.cacert.overrideDerivation (drv: {
          installPhase = (drv.installPhase or "") + ''
            cat "${nodes.ca.config.headcounter.snakeOilCaCert}" \
              >> "$out/etc/ssl/certs/ca-bundle.crt"
          '';
        });
      };
      networking.nameservers = lib.mkForce [
        nodes.resolver.config.networking.primaryIPAddress
      ];
    };

  in {
    dns = { config, pkgs, lib, nodes, ... }: {
      environment.systemPackages = [ pkgs.socat ];
      services.nsd.enable = true;
      services.nsd.interfaces = lib.mkForce []; # all interfaces
      headcounter.services.acme.dnsHandler = {
        enable = true;
        fqdn = "ns.example.com";
        listen = lib.singleton { host = "10.0.0.2"; device = "tun0"; };
      };
      services.nsd.zones."example.com.".data = let
        webserverIp = nodes.webserver.config.networking.primaryIPAddress;
      in ''
        $ORIGIN example.com.
        @ IN SOA ns.example.com. admin.example.com. (
          0       ; Serial
          28800   ; Refresh
          7200    ; Retry
          604800  ; Expire
          86400   ; Negative Cache TTL
        )

        @ IN NS ns
        ns IN A ${config.networking.primaryIPAddress}

        @ IN A ${webserverIp}
        * IN A ${webserverIp}

        _acme-challenge IN NS ns
      '';
    };

    ca = { nodes, lib, ... }: {
      imports = [ ../modules/testing/letsencrypt.nix ];
      networking.nameservers = lib.mkForce [
        nodes.resolver.config.networking.primaryIPAddress
      ];
    };

    resolver = ../modules/testing/resolver.nix;

    client = { pkgs, ... }: {
      imports = [ common ];
      environment.systemPackages = [ pkgs.openssl ];
    };

    webserver = { lib, pkgs, ssl, nodes, ... }: {
      imports = [ common ];

      users.users.attacker = {
        isNormalUser = true;
        description = "Attacker";
      };

      environment.systemPackages = [ pkgs.socat ];

      networking.extraHosts = let
        caIp = nodes.ca.config.networking.primaryIPAddress;
      in "${caIp} acme-v01.api.letsencrypt.org letsencrypt.org";

      users.users.twistd.group = "twistd";
      users.groups.twistd = {};

      systemd.services.twistd = {
        description = "SSL Test Webserver";
        after = [ "network.target" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = let
          twistedSSL = pkgs.python.buildEnv.override {
            extraLibs = with pkgs.pythonPackages; [ pyopenssl twisted ];
          };
          docroot = pkgs.runCommand "docroot" {} ''
            mkdir "$out"
            echo hello twistd > "$out/index.html"
          '';
        in {
          User = "twistd";
          Group = "twistd";
          PrivateTmp = true;
          AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
          ExecStart = toString [
            "${twistedSSL}/bin/twistd" "-no"
            "--pidfile=/tmp/dummy.pid" "web"
            "-c" ssl."example.com".cert
            "-k" ssl."example.com".privkey
            "--https=443"
            "--path=${docroot}"
          ];
        };
      };

      headcounter.services.lighttpd.enable = true;
      headcounter.services.lighttpd.virtualHosts = lib.singleton {
        type = "static";
        on = "www.example.com";
        docroot = pkgs.runCommand "docroot" {} ''
          mkdir "$out"
          echo hello lighty > "$out/index.html"
        '';
        socket = ":444";

        socketConfig = ''
          ssl.engine    = "enable"
          ssl.use-sslv2 = "disable"
          ssl.use-sslv3 = "disable"
          ssl.pemfile = "${ssl."example.com".full}"
        '';
      };

      headcounter.services.acme.enable = true;
      headcounter.services.acme.handlerAddress = "10.0.0.2";
      headcounter.services.acme.handlerDevice = "tun0";
      headcounter.services.acme.domains."example.com" = {
        users = [ "twistd" ];
        restarts = [ "twistd" "lighttpd" ];
        otherDomains = [ "www.example.com" ];
      };
    };
  };

  testScript = ''
    sub checkperms {
      $webserver->nest("check permissions of private keys", sub {
        my $files = $webserver->succeed(
          'find /var/lib/acme \( -name full -o -name privkey \) -print'
        );
        chomp $files;
        for my $file (split /\n/, $files) {
          $webserver->fail('su -c \'cat "'.$file.'"\' attacker >&2');
        }
      });
    }

    $resolver->start;
    $dns->start;
    $ca->start;
    $client->start;

    $resolver->waitForUnit("bind.service");
    $dns->waitForUnit("nsd.service");
    $ca->waitForUnit("boulder.service");

    # Intentionally provoke race conditions
    $webserver->waitForUnit("twistd.service");

    $webserver->nest('establish tunnel to nameserver', sub {
      $webserver->succeed('socat TCP-LISTEN:1111 TUN:10.0.0.1/8,up &');
      $webserver->waitUntilSucceeds('netstat -ntl | grep -q ":1111\\>"');
      $dns->succeed('socat TCP:webserver:1111 TUN:10.0.0.2/24,up &');
    });

    $webserver->waitForOpenPort(443);
    $webserver->waitForOpenPort(444);

    $client->waitForUnit("multi-user.target");

    $client->succeed(
      'curl https://example.com/ | grep -q "hello twistd"',
      'curl https://www.example.com:444/ | grep -q "hello lighty"'
    );

    checkperms;

    my $newdateUnix = $client->succeed('date +%s --date "80 days"');
    chomp $newdateUnix;
    my $newdate = $client->succeed('date +%m%d%H%M%Y --date @'.$newdateUnix);
    chomp $newdate;

    $log->nest("fast-forward time to now + 80 days", sub {
      $_->execute('date '.$newdate) foreach values %vms;
    });

    $ca->succeed('systemctl stop boulder*');
    $ca->succeed('systemctl start boulder*');
    $ca->succeed('systemctl start boulder');
    $ca->waitForUnit("boulder.service");

    $log->nest("wait for ACME reissuance", sub {
      my $getTwistdStartTime =
        'systemctl show -p ExecMainStartTimestampMonotonic twistd.service';

      my $twistdStartTime = $webserver->succeed($getTwistdStartTime);

      $webserver->execute('systemctl start acme.service');

      $webserver->nest("wait until twistd has restarted", sub {
        Machine::retry sub {
          return 1
            if $webserver->succeed($getTwistdStartTime) ne $twistdStartTime;
        };
      });
      $webserver->waitForOpenPort(443);
      $webserver->waitForOpenPort(444);
    });

    $client->succeed(
      'curl https://example.com/ | grep -q "hello twistd"',
      'curl https://www.example.com:444/ | grep -q "hello lighty"'
    );

    for my $host ("example.com:443", "www.example.com:444") {
      my $issuanceUnix = $client->succeed(
        'fetched="$(echo | openssl s_client -connect '.$host.')" && '.
        'parsed="$(echo "$fetched" | openssl x509 -noout -dates)" && '.
        'mangled="$(echo "$parsed" | sed -ne "s/notBefore=//p")" && '.
        'date --date "$mangled" +%s'
      );

      my $issuance = $client->succeed('date --date @'.$issuanceUnix);
      chomp $issuance;

      $issuanceUnix > $newdateUnix - 7200
        or die "Certificate is too old ($issuance)";
    }

    checkperms;
  '';
}
