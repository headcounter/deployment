{ pkgs, ... }:

{
  nodes = {
    ultron = {
      imports = import ../modules/module-list.nix ++ [
        ../xmpp.nix ../domains.nix
      ];
      headcounter.useSnakeOil = true;
    };
    benteflork = {};
  };

  testScript = ''
    startAll;
    $ultron->waitForUnit("mongooseim.service");
  '';
}
