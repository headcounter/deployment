import ./make-test.nix {
  name = "postfix";

  nodes = {
    server = {
      imports = [ ../common.nix ];
      headcounter.services.postfix.enable = true;
    };
    client = { pkgs, ... }: {
      imports = [ ../common.nix ];
      environment.systemPackages = [ pkgs.swaks ];
    };
  };

  testScript = ''
    startAll;

    $server->waitForOpenPort(25);
    $client->succeed('swaks --to root@server');
  '';
}
