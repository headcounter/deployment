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
    dns = { config, lib, nodes, ... }: {
      services.nsd.enable = true;
      services.nsd.interfaces = lib.mkForce []; # all interfaces
      headcounter.services.acme.dnsHandler = {
        enable = true;
        fqdn = "ns.example.com";
        listen = lib.singleton { host = "0.0.0.0"; };
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

    client = common;

    webserver = { lib, pkgs, ssl, nodes, ... }: {
      imports = [ common ];

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
            echo hello world > "$out/index.html"
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

      headcounter.services.acme.enable = true;
      headcounter.services.acme.handlerAddress = "dns";
      headcounter.services.acme.domains."example.com".users = [ "twistd" ];
    };
  };

  testScript = ''
    $resolver->start;
    $dns->start;
    $ca->start;
    $client->start;

    $resolver->waitForUnit("bind.service");
    $dns->waitForUnit("nsd.service");
    $ca->waitForUnit("boulder.service");

    $webserver->waitForOpenPort(443);

    $client->waitForUnit("multi-user.target");
    $client->succeed('curl https://example.com/ >&2');
  '';
}
