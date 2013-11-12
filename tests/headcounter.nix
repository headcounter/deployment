{ pkgs, ... }:

{
  nodes = {
    ultron = {
      imports = import ../modules/module-list.nix ++ [
        ../xmpp.nix ../domains.nix
      ];
    };
    benteflork = {};
  };

  testScript = ''
    startAll;
    $ultron->waitForUnit("mongooseim.service");
  '';
}
