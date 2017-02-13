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
      patchedPoke pkgs.headcounter.xmppokeReport
    ];
  };

  testScript = { nodes, ... }: let
    inherit (nodes.ultron.config.headcounter.vhosts.${vhost}) fqdn;
    inherit (import ../../../ssl/snakeoil.nix fqdn) rootCAFile;
    pokeOpts = "--cafile=${rootCAFile} --delay=0";

    resultSql = "SELECT sr.total_score, sr.grade, tr.type"
              + "  FROM srv_results sr, test_results tr"
              + "  WHERE sr.test_id = tr.test_id";
    getResult = "echo '${resultSql}' | psql -t -Pformat=unaligned -F: xmppoke";
  in ''
    $client->waitForUnit("network.target");

    $client->nest("check availability", sub {
      $client->succeed("ping -c1 ${fqdn} >&2");
      $client->succeed("nc -z ${fqdn} 5222");
    });

    $client->waitForUnit("postgresql.service");

    subtest "xmppoke", sub {
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
    };
  '';
}
