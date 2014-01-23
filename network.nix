let
  mkMachine = attrs: attrs // {
    imports = import modules/module-list.nix
           ++ [ ./common.nix ]
           ++ attrs.imports or [];
  };
in {
  network.description = "Headcounter Services";
  network.enableRollback = true;

  resources.sshKeyPairs."hydra-build" = {};

  ultron = { pkgs, ... }: mkMachine {
    imports = [ ./machines/ultron.nix ];
    deployment.hetzner.mainIPv4 = "5.9.105.142";

    systemd.services."legacy-portfw" = {
      description = "Port forwarding to old server";
      after = [ "network-interfaces.target" ];
      before = [ "network.target" ];
      wantedBy = [ "network.target" ];

      path = [ pkgs.iptables ];

      serviceConfig.Type = "oneshot";
      serviceConfig.RemainAfterExit = true;

      script = with pkgs.lib; ''
        ${flip concatMapStrings [ 5222 5223 5269 ] (port: ''
        iptables -t nat -A PREROUTING -p tcp --dport ${toString port} \
          -j DNAT --to-destination 88.198.198.219:${toString port}
        iptables -t nat -A POSTROUTING -p tcp --dport ${toString port} \
          -j MASQUERADE
        '')}
        echo 1 > /proc/sys/net/ipv4/ip_forward
      '';
    };
  };

  taalo = { pkgs, config, ... }: mkMachine {
    imports = [ ./hydra.nix ./chromium.nix ];
    deployment.hetzner.mainIPv4 = "188.40.96.202";
    deployment.encryptedLinksTo = [ "ultron" ];
    services.hydra.listenHost = pkgs.lib.mkForce
      config.networking.p2pTunnels.ssh.ultron.localIPv4;
  };

  benteflork = mkMachine {
    imports = [ ./hydra-slave.nix ];
    deployment.hetzner.mainIPv4 = "144.76.202.147";
  };
}
