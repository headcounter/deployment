import ./make-test.nix {
  name = "postfix";

  nodes = {
    server = {
      headcounter.services.postfix.enable = true;
    };
    client = { pkgs, ... }: {
      environment.systemPackages = [ pkgs.swaks ];
    };
  };

  testScript = ''
    startAll;

    $server->waitForOpenPort(25);
    $client->succeed('swaks --to root@server');
  '';
}
