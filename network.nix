{
  network.description = "Headcounter Services";
  network.enableRollback = true;

  resources.sshKeyPairs."hydra-build" = {};

  ultron = { pkgs, config, ... }: {
    imports = [ ./common.nix ./machines/ultron.nix ];
    deployment.hetzner.mainIPv4 = "5.9.105.142";
  };

  taalo = { pkgs, config, ... }: {
    imports = [ ./common.nix ./hydra-slave.nix ];
    deployment.hetzner.mainIPv4 = "144.76.61.117";
  };

  benteflork = { pkgs, config, ... }: {
    imports = [ ./common.nix ./hydra-slave.nix ];
    deployment.hetzner.mainIPv4 = "144.76.202.147";
  };
}
