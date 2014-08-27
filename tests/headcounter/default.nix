import <nixpkgs/nixos/tests/make-test.nix> ({ pkgs, ... }:

{
  nodes = {
    ultron = {
      imports = import ../../modules/module-list.nix ++ [
        ../../xmpp.nix ../../domains.nix
      ];
      headcounter.useSnakeOil = true;
      users.extraUsers.mongoose.extraGroups = [ "keys" ];
      networking.firewall.enable = false;
    };
    benteflork = {
      networking.firewall.enable = false;
    };
  };

  testScript = ''
    startAll;
    $ultron->waitForUnit("mongooseim.service");
  '';
})
