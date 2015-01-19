{ fqdn, ... }:

{
  testScript = ''
    $client->nest("check availability", sub {
      $client->succeed("ping -c1 ${fqdn} >&2");
      $client->succeed("nc -z ${fqdn} 5222");
    });
    $client->succeed("xmppoke ${fqdn} >&2");
  '';
}
