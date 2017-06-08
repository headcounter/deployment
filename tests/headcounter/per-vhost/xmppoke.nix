vhost:

{
  name = "vhost-${vhost}-xmppoke";

  nodes.client = { pkgs, lib, ... }: let
    patchedPoke = lib.overrideDerivation pkgs.headcounter.xmppoke (o: {
      postPatch = (o.postPatch or "") + ''
        sed -ri -e 's/(db_host *= *)[^,]*/\1nil/' \
                -e '/Connecting to database/d' \
                poke.lua
      '';
    });
  in {
    services.postgresql = {
      enable = true;
      package = pkgs.postgresql;
      initialScript = pkgs.writeText "init.sql" ''
        CREATE ROLE xmppoke WITH LOGIN;
        CREATE DATABASE xmppoke OWNER xmppoke;
        \c xmppoke
        BEGIN;
        \i ${patchedPoke}/share/xmppoke/schema.pg.sql
        COMMIT;
      '';
      authentication = ''
        local all xmppoke trust
      '';
    };

    environment.systemPackages = [
      patchedPoke pkgs.headcounter.xmppokeReport pkgs.openssl
    ];
  };

  excludeNodes = [ "taalo" "benteflork" "unzervalt" ];

  testScript = { nodes, ... }: let
    inherit (nodes.ultron.config.headcounter.vhosts.${vhost}) fqdn;
    pokeOpts = "--cafile=/etc/ssl/certs/ca-bundle.crt --delay=0";

    resultSql = "SELECT sr.total_score, sr.grade, tr.type"
              + "  FROM srv_results sr, test_results tr"
              + "  WHERE sr.test_id = tr.test_id";
    getResult = "echo '${resultSql}' | psql -t -Pformat=unaligned -F: xmppoke";
  in ''
    $client->waitForUnit("network.target");

    my $srvfqdn;

    $client->nest("check availability", sub {
      my $srvreply = $client->succeed("host -t srv _xmpp-server._tcp.${fqdn}");
      $srvreply =~ /has SRV record(?:\s+\S+){3}\s+(\S+)\.$/m;
      $srvfqdn = $1;
      $client->succeed("ping -c1 $srvfqdn >&2");
      $client->succeed("nc -z $srvfqdn 5222");
    });

    $client->waitForUnit("postgresql.service");

    $client->nest("force ACME certificate renewal", sub {
      my $newdateUnix = $client->succeed('date +%s --date "80 days"');
      chomp $newdateUnix;
      my $newdate = $client->succeed('date +%m%d%H%M%Y --date @'.$newdateUnix);
      chomp $newdate;

      $log->nest("fast-forward time to now + 80 days", sub {
        $_->execute('date '.$newdate) foreach values %vms;
      });

      $ca->succeed('systemctl restart boulder*');

      $ultron->execute('systemctl start acme.service');

      $client->waitUntilSucceeds(
        'echo | openssl s_client -connect '.$srvfqdn.':5223'
      );

      my $issuanceUnix = $client->succeed(
        'fetched="$(echo | openssl s_client -connect '.$srvfqdn.':5223)" && '.
        'parsed="$(echo "$fetched" | openssl x509 -noout -dates)" && '.
        'mangled="$(echo "$parsed" | sed -ne "s/notBefore=//p")" && '.
        'date --date "$mangled" +%s'
      );

      my $issuance = $client->succeed('date --date @'.$issuanceUnix);
      chomp $issuance;

      $issuanceUnix > $newdateUnix - 7200
        or die "Certificate is too old ($issuance)";
    });

    $client->nest("xmppoke", sub {
      $client->succeed("xmppoke ${pokeOpts} --mode=client '${fqdn}' >&2");
      $client->succeed("xmppoke ${pokeOpts} --mode=server '${fqdn}' >&2");

      $client->succeed(
        "mkdir /tmp/xchg/xmppoke && (cd /tmp/xchg/xmppoke; ".
        "xmppoke-genreport client '${fqdn}'; ".
        "xmppoke-genreport server '${fqdn}'".
        "); sync"
      );

      system("tar xf vm-state-client/xchg/xmppoke/client.tar -C '$out'");
      system("tar xf vm-state-client/xchg/xmppoke/server.tar -C '$out'");

      open HYDRA, ">>$out/nix-support/hydra-build-products";
      print HYDRA "report tls-c2s $out/xmppoke client.html\n";
      print HYDRA "report tls-s2s $out/xmppoke server.html\n";
      close HYDRA;

      my $failed = 2;
      my $result = $client->succeed("${getResult}");
      foreach (split /\n/, $result) {
        my @sr = split /:/;
        if ($sr[1] eq 'A') {
          $failed--;
        } else {
          $client->log("The $sr[2] test passed with a grade of ".
                       "$sr[1] instead of A!");
        }
      }
      if ($failed) {
        open TOUCH_FAILED, ">>$out/nix-support/failed";
        close TOUCH_FAILED;
      }
    });
  '';
}
