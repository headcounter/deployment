import ./make-test.nix {
  name = "postfix";

  machine = {
    imports = [ ../common.nix ];
    headcounter.services.postfix.enable = true;
  };

  # TODO: Only a dummy test for now
  testScript = ''
    startAll;
    $machine->waitForUnit("multi-user.target");
  '';
}
