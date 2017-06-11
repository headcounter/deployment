import ./make-test.nix ({ lib, ... }: {
  name = "postgresql";

  machine = { config, pkgs, lib, ... }: {
    services.postgresql.enable = true;

    users.users.tester.isNormalUser = true;
    users.users.alice.isNormalUser = true;
    users.users.bob.isNormalUser = true;

    headcounter.postgresql.databases = let
      schemaFile = pkgs.writeText "create-lookup.sql" ''
        CREATE TABLE lookup (id SERIAL);
      '';
    in {
      foo = {
        extraSql = ''
          CREATE SCHEMA private;
          CREATE SEQUENCE private.fooseq;
          CREATE TABLE private.bar (id SERIAL);
          CREATE TABLE foo (value TEXT);
        '';

        inherit schemaFile;

        users.alice.permissions = lib.mkOptionDefault {
          schema.private.all = true;
          allSequences.private.all = true;
          allTables.private.all = true;
        };

        users.bob.permissions = lib.mkForce {
          table.public.foo.select = true;
          table.private.bar.insert = true;
          sequence.private.fooseq.select = true;
          schema.public.usage = true;
          schema.private.usage = true;
        };

        users.tester.permissions = lib.mkForce {
          schema.public.usage = true;
          table.public.foo.insert = true;
        };

        neededBy = [ "testdb.service" ];
      };

      bar = {
        extraSql = ''
          CREATE SCHEMA secret;
          CREATE TABLE secret.testme (value TEXT);
          INSERT INTO secret.testme VALUES('magic');
        '';

        inherit schemaFile;

        users.tester.permissions = lib.mkForce {
          table.secret.testme.select = true;
          schema.secret.usage = true;
        };

        neededBy = [ "testdb.service" ];
      };
    };

    systemd.services.testdb = {
      description = "Test PostgreSQL Database `bar'";
      wantedBy = [ "multi-user.target" ];
      serviceConfig.RemainAfterExit = true;
      serviceConfig.User = "tester";
      path = [ config.services.postgresql.package ];
      script = ''
        value="$(psql -tAq -c 'SELECT * FROM secret.testme' bar)"
        psql -c "INSERT INTO foo VALUES('$value');" foo
      '';
    };
  };

  testScript = ''
    ${lib.concatMapStrings (what: ''
      sub sql${what} ($$$) {
        my ($user, $db, $sql) = @_;
        $sql =~ s/'/'\\'''/g;
        my $cmd = "psql -c '$sql' $db";
        $cmd =~ s/'/'\\'''/g;
        return $machine->${lib.toLower what}("su -c '$cmd' $user");
      }
    '') [ "Succeed" "Fail" ]}

    $machine->waitForUnit('testdb.service');
    $machine->waitForUnit('multi-user.target');

    sqlSucceed "alice", "foo", "SELECT setval('private.fooseq', 1)";

    sqlFail "bob", "foo", "INSERT INTO foo VALUES('123')";

    sqlSucceed("bob", "foo", "SELECT * FROM foo") =~ /magic/
      or die "No magic found in table foo.public.foo.";

    sqlFail "bob", "foo", "SELECT * FROM private.bar";
    sqlSucceed "bob", "foo", "INSERT INTO private.bar VALUES(321)";
    sqlSucceed "bob", "foo", "SELECT last_value FROM private.fooseq";
    sqlFail "bob", "foo", "SELECT nextval('private.fooseq')";

    sqlFail "bob", "bar", "SELECT * FROM lookup";

    sqlSucceed "tester", "bar", "SELECT * FROM secret.testme";
    sqlFail "tester", "foo", "SELECT * FROM foo";

    sqlSucceed "alice", "foo", "SELECT * FROM lookup";
    sqlFail "alice", "bar", "SELECT * FROM lookup";

    sqlSucceed "alice", "foo", "INSERT INTO foo VALUES('abc')";
    sqlSucceed "alice", "foo", "INSERT INTO private.bar VALUES(456)";
    sqlSucceed "alice", "foo", "SELECT last_value FROM private.fooseq";
    sqlFail "alice", "foo", "CREATE TABLE xyz (id SERIAL)";

    sqlSucceed "alice", "foo", "CREATE TABLE private.uvw (id SERIAL)";
  '';
})
