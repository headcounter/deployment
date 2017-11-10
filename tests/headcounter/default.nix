{ lib, ... }@passthru:

let
  # This is for all machines in the deployment, because when run on Hydra, all
  # of the deployment tests are run at once which causes a lot of services to
  # time out because of the high load.
  timeoutConfig = { lib, ... }: {
    systemd.extraConfig = ''
      DefaultTimeoutStartSec=600s
      DefaultTimeoutStopSec=600s
    '';
    systemd.services.postgresql.serviceConfig.TimeoutSec = lib.mkForce 600;
  };

  # All the machines in the deployment.
  deployment = lib.mapAttrs (node: config: {
    imports = [ config ../../modules/testing/nixops.nix timeoutConfig ];

    config = {
      headcounter.nixops = {
        inherit (import ../../network.nix) resources;
      };
    } // lib.optionalAttrs (node == "ultron") {
      virtualisation.memorySize = 2048;
    };
  }) (builtins.removeAttrs (import ../../network.nix) [
    "network" "defaults" "resources" "require" "_file"
  ]);

  # Common config for *all* nodes in the deployment.
  commonConfig = { config, lib, nodes, ... }: {
    imports = [ ../../modules/testing/network.nix ];

    nixpkgs.overlays = lib.singleton (lib.const (super: {
      cacert = super.cacert.overrideDerivation (drv: {
        installPhase = (drv.installPhase or "") + ''
          cat "${nodes.ca.config.headcounter.snakeOilCaCert}" \
            >> "$out/etc/ssl/certs/ca-bundle.crt"
        '';
      });
    }));

    networking.nameservers = lib.mkForce [
      nodes.resolver.config.networking.primaryIPAddress
    ];

    networking.extraHosts = let
      caIp = nodes.ca.config.networking.primaryIPAddress;
    in "${caIp} acme-v01.api.letsencrypt.org letsencrypt.org";
  };

  makeHeadcounterTest = maybeExpr: import ../make-test.nix ({ lib, ... }: let
    isDirect = builtins.isFunction maybeExpr || builtins.isAttrs maybeExpr;
    expr = if isDirect then maybeExpr else import maybeExpr;
    attrs = if builtins.isFunction expr then expr passthru else expr;

    nodes = lib.zipAttrsWith (node: cfgs: {
      imports = cfgs ++ [ commonConfig ];
    }) [ deployment (attrs.nodes or {}) ];

    excludeNodes = attrs.excludeNodes or [];
  in {
    name = "headcounter-${attrs.name}";

    nodes = nodes // {
      # Support node needed for representing the letsencrypt CA.
      ca = { nodes, lib, ... }: {
        imports = [ ../../modules/testing/letsencrypt.nix commonConfig ];
        networking.nameservers = lib.mkForce [
          nodes.resolver.config.networking.primaryIPAddress
        ];
      };

      torservers = { pkgs, lib, ... }: {
        imports = [ commonConfig ];

        headcounter.mainIPv4 = "81.7.6.108";
        headcounter.mainIPv6 = "2a02:180:a:25:5::1";

        nixpkgs.overlays = lib.singleton (self: super: {
          pythonPackages = (super.python.override {
            packageOverrides = lib.const (pysuper: {
              certifi = pysuper.certifi.overridePythonAttrs (attrs: {
                postPatch = (attrs.postPatch or "") + ''
                  cat "${self.cacert}/etc/ssl/certs/ca-bundle.crt" \
                    > certifi/cacert.pem
                '';
              });
            });
          }).pkgs;
        });

        services.nginx.enable = true;
        services.nginx.virtualHosts."torservers.net" = {
          enableACME = true;
          forceSSL = true;
          root = pkgs.runCommand "torservers-docroot" {} ''
            mkdir "$out"
            echo "torservers.net site" > "$out/index.html"
          '';
        };
      };

      # A dummy DNS root resolver.
      resolver = { nodes, pkgs, lib, ... }: {
        imports = [ ../../modules/testing/resolver.nix commonConfig ];
        # Extra zone for torservers.net that we don't control.
        services.bind.zones = lib.singleton {
          name = "torservers.net";
          file = pkgs.writeText "torservers.zone" ''
            @ IN SOA ns.fakedns. hostmaster.headstrong.de. (
              2016122302 10800 3600 604800 3600
            )
            @ IN NS ns.fakedns.

            @ IN A    ${nodes.torservers.config.headcounter.mainIPv4}
            @ IN AAAA ${nodes.torservers.config.headcounter.mainIPv6}

            jabber IN NS ns1.headcounter.org.
            jabber IN NS ns2.headcounter.org.

            _acme-challenge IN NS ns1.headcounter.org.

            _xmpp-server._tcp IN SRV 0 0 5269 jabber
            _xmpp-client._tcp IN SRV 0 0 5222 jabber
          '';
        };
      };
    };

    testScript = scriptAttrs: let
      subTestScript = if builtins.isFunction attrs.testScript
                      then attrs.testScript scriptAttrs
                      else attrs.testScript;

      runForDeplMachines = command: let
        runCommand = machine: "\$${machine}->${command};";
        machines = lib.attrNames (removeAttrs deployment excludeNodes);
      in lib.concatMapStrings runCommand machines;

      startRest = let
        runCommand = node: "\$${node}->start;";
        others = lib.attrNames (removeAttrs nodes (lib.attrNames deployment));
      in lib.concatMapStrings runCommand others;
    in ''
      my $out = $ENV{'out'};

      $log->nest("start up ACME infrastructure", sub {
        $resolver->start;
        $ca->start;

        $resolver->waitForUnit("bind.service");
        $ca->waitForUnit("boulder.service");
      });

      $log->nest("start up torservers.net", sub {
        $torservers->waitForUnit("acme-certificates.target");
        $torservers->waitUntilSucceeds(
          'curl https://torservers.net | grep -qF "torservers.net site"'
        );
      });

      $log->nest("start up DNS servers", sub {
        $dugee->start;
        $gussh->start;
        $dugee->waitForUnit('multi-user.target');
        $gussh->waitForUnit('multi-user.target');
      });

      $log->nest("start up all deployment nodes", sub {
        ${runForDeplMachines "start"}
        ${runForDeplMachines "waitForUnit('multi-user.target')"}
      });

      $log->nest("start up test nodes", sub {
        ${startRest}
      });

      ${subTestScript}
    '';
  } // removeAttrs attrs [ "name" "nodes" "testScript" ]) passthru;

  testedVHosts = [ "headcounter" "aszlig" "noicq" "no_icq" "torservers" ];

in {
  vhosts = lib.genAttrs testedVHosts (vhost: {
    xmppoke = makeHeadcounterTest (import ./per-vhost/xmppoke.nix vhost);
    escalus = makeHeadcounterTest (import ./per-vhost/escalus vhost);
  });

  listeners = makeHeadcounterTest ./listeners.nix;
}
