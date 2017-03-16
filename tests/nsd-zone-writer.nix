import ./make-test.nix ({ lib, ... }: {
  name = "nsd-zone-writer";

  machine = {
    services.nsd = {
      enable = true;
      interfaces = lib.mkForce [];
      zones."dummy.".data = "@ SOA ns.dummy noc.dummy ( 1 1h 1h 1h 1h )\n";
    };
    headcounter.nsd-zone-writer = {
      enable = true;
      allowedUsers = [ "alice" ];
    };
    users.users = {
      alice.isNormalUser = true;
      bob.isNormalUser = true;
    };
  };

  testScript = { nodes, ... }: let
    writeZone = nodes.machine.config.headcounter.nsd-zone-writer.command;
    writeTestZone = cmd: user: fqdn: let
      testZone = ''
        @ SOA ns.${fqdn}. noc.${fqdn}. ( 1 3h 1h 1w 1d )
        test IN A 1.2.3.4
      '';
      userCmd = "echo ${lib.escapeShellArg testZone} | ${writeZone} ${fqdn}";
      shellCmd = "su -c ${lib.escapeShellArg userCmd} ${user}";
    in "\$machine->${cmd}('${lib.escape ["'" "\\"] shellCmd}');";
  in ''
    sub checkZone ($) {
      my ($fqdn) = @_;
      my $out = $machine->succeed('host test.'.$fqdn.' 127.0.0.1');
      chomp $out;
      $out =~ /^test\.\Q$fqdn\E\s+A\s+\Q1.2.3.4\E$/
        or die "Zone for $fqdn not available (output: '$out')!";
    }

    sub checkStat ($$$$) {
      my ($path, $modes, $owner, $group) = @_;
      my $out = $machine->succeed('stat -c %A:%U:%G '.$path);
      chomp $out;
      my ($oModes, $oOwner, $oGroup) = split /:/, $out;
      my $onPath = " on path $path ";
      $modes eq $oModes or die "Expected modes$onPath$modes but got $oModes!";
      $owner eq $oOwner or die "Expected owner$onPath$owner but got $oOwner!";
      $group eq $oGroup or die "Expected owner$onPath$group but got $oGroup!";
    }

    $machine->waitForUnit('multi-user.target');
    checkStat "/var/lib/nsd", "drwx--x---", "nsd", "dynzone";
    checkStat "/var/lib/nsd/dynzones", "drwx--s---", "dynzone", "nsd";
    ${writeTestZone "succeed" "alice" "foo"}
    checkZone "foo";
    checkStat "/var/lib/nsd/dynzones/foo.zone", "-rw-r-----", "dynzone", "nsd";
    ${writeTestZone "fail" "bob" "bar"}
    $machine->fail('stat /var/lib/nsd/dynzones/bar.zone');
  '';
})
