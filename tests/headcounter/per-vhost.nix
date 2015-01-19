{ fqdn, ... }:

let
  inherit (import ../../ssl/snakeoil.nix fqdn) rootCA;
  pokeOpts = "--output=xmppoke --html --cafile=${rootCA}/root.pem";
in {
  testScript = ''
    $client->nest("check availability", sub {
      $client->succeed("ping -c1 ${fqdn} >&2");
      $client->succeed("nc -z ${fqdn} 5222");
    });

    subtest "xmppoke", sub {
      $client->succeed("mkdir xmppoke");
      $client->succeed("xmppoke ${pokeOpts} --mode=client '${fqdn}' >&2");
      $client->succeed("xmppoke ${pokeOpts} --mode=server '${fqdn}' >&2");

      $client->succeed('tar cf /tmp/xchg/xmppoke.tar xmppoke && sync');
      system("tar xf vm-state-client/xchg/xmppoke.tar -C '$out'");

      open HYDRA, ">>$out/nix-support/hydra-build-products";
      print HYDRA "report tls-c2s $out/xmppoke client-aszlig.net.html\n";
      print HYDRA "report tls-s2s $out/xmppoke server-aszlig.net.html\n";
      close HYDRA;
    };
  '';
}
