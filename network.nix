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

  ultron = mkMachine {
    imports = [ ./machines/ultron.nix ];
    deployment.hetzner.mainIPv4 = "5.9.105.142";
  };

  taalo = mkMachine {
    imports = [ ./hydra-slave.nix ];
    deployment.hetzner.mainIPv4 = "144.76.61.117";
  };

  benteflork = mkMachine {
    imports = [ ./hydra-slave.nix ];
    deployment.hetzner.mainIPv4 = "144.76.202.147";
  };
}
