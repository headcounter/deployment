{ pkgs, lib, ... }:

{
  runInCtl = machine: commands: let
    expectScript = pkgs.writeScript "mongooseimctl.expect" ''
      #!${pkgs.expect}/bin/expect -f
      set timeout 60

      proc sendCommand {command} {
        expect -ex {** exception} {
          exit 1
        } -re {\(mongooseim@${machine}\)\d+>} {
          send -- "$command\r"
        } timeout {
          puts "Erlang shell isn't connected to remote node!"
          exit 1
        }
      }

      set commands [open [lindex $argv 0] r]
      spawn mongooseimctl debug
      while {-1 != [gets $commands line]} {
        sendCommand $line
      }
      close $commands
      sendCommand \007
      expect -re {-->}
      send "q\r"
      exit [lindex [wait] 3]
    '';
    cmdFile = pkgs.writeText "ctl-commands.erl" commands;
  in "\$${machine}->succeed('${expectScript} ${cmdFile} >&2');";

  checkListeners = node: let
    inherit (node.config.headcounter) services;
    inherit (services.mongooseim) configFile;
    hclib = import ../../lib { inherit lib; };
    isLocalEpmd = services.epmd.addresses == lib.singleton "127.0.0.1";
    PortAndAddr = "{127, 0, 0, 1, KernelPort}";
    kernelPortOrAddrInfo = if isLocalEpmd then "KernelPort" else PortAndAddr;
  in ''
    {ok, Config} = file:consult(${hclib.erlString configFile}),

    KernelPid = erlang:process_info(whereis(net_kernel)),
    KernelLinks = proplists:get_value(links, KernelPid),
    KernelPorts = [inet:port(Port) || Port <- KernelLinks, is_port(Port)],
    {ok, KernelPort} = hd(KernelPorts),

    CfgListeners = [element(1, L) ||
                    L <- proplists:get_value(listen, Config, [])],
    Listeners = [KernelPort|CfgListeners],
    Split = lists:partition(fun({_, _}) -> true; (_) -> false end, Listeners),
    {AllowedHostPort, AllowedPorts} = Split,
    IsAllowed = fun(Sock) ->
        {ok, {Host, Port}} = inet:sockname(Sock),
        lists:member({Port, Host}, AllowedHostPort) or
            lists:member(Port, AllowedPorts)
    end,
    IsListening = fun(Port) ->
        {ok, Status} = prim_inet:getstatus(Port),
        lists:member(listen, Status)
    end,
    Recurse = fun
        (_, Pid, 3) -> Pid;
        (Self, Pid, Inc) ->
            Info = erlang:process_info(Pid),
            Links = proplists:get_value(links, Info, []),
            NewLinks = [Self(Self, Link, Inc + 1) ||
                        Link <- Links, is_pid(Link)],
            lists:keyreplace(links, 1, Info, {links, NewLinks})
    end,
    GetDeepProcessInfo = fun(Pid) -> Recurse(Recurse, Pid, 1) end,
    GetSockInfo = fun(Port) ->
        {ok, AddrInfo} = inet:sockname(Port),
        {links, Links} = erlang:port_info(Port, links),
        [[{listen_at, AddrInfo}|GetDeepProcessInfo(Pid)] || Pid <- Links]
    end,
    Ports = [GetSockInfo(Port) || Port <- erlang:ports(),
             erlang:port_info(Port, name) == {name, "tcp_inet"},
             IsListening(Port), not IsAllowed(Port)],
    case Ports of
        [] -> ok;
        _  -> io:format("Stray listeners: ~p~n", [Ports]),
              erlang:error(unacceptable)
    end.
  '';

  # Returns a shell script fragment for use in testScript during VM tests, which
  # runs an Erlang Common Test runner that is passed as a list of command line
  # arguments (which should not be escaped already) and it makes sure the VM
  # test derivation doesn't fail.
  #
  # The reason why we don't want it to fail is because we are creating
  # $out/nix-support/failed, which causes Hydra to mark the job as failed but we
  # still get an output (which in this case is the HTML report of the CT run).
  #
  # After the runner command has executed the report is expected to be in
  # /tmp/ct_report (/tmp is the pwd, so a relative "ct_report" should suffice).
  runCommonTests = runnerCmdline: let
    runner = lib.concatMapStringsSep " "lib.escapeShellArg runnerCmdline;
  in ''
    my $testCmd = 'mkdir -p ct_report && ${lib.escape ["'"] runner} >&2';

    $client->nest("running test suite: $testCmd", sub {
      my $rval = ($client->execute_($testCmd))[0];
      my $out = $ENV{'out'};

      my $rawugly = $client->succeed(
        'find ct_report -name \'*@*\' -print | '.
        'xargs -I{} sh -c \'mv "{}" "$(echo "{}" | '.
        'tr @ _)" && basename "{}"\' '
      );
      chomp $rawugly;
      my @uglynames = split "\n", $rawugly;
      foreach my $ugly (@uglynames) {
        $client->succeed('find ct_report -type f -exec '.
                         "sed -i -e 's|$ugly|".($ugly =~ s/\@/_/gr)."|' {} +");
      }

      $client->succeed('tar cf /tmp/xchg/ct_report.tar ct_report && sync');
      system("tar xf vm-state-client/xchg/ct_report.tar -C '$out'");

      open HYDRA_PRODUCTS, ">>$out/nix-support/hydra-build-products";
      print HYDRA_PRODUCTS "report ct-tests $out/ct_report\n";
      close HYDRA_PRODUCTS;

      my @summaries = <$out/ct_report/ct_run.*/*.logs/run.*/suite.summary>;
      my @stats;
      foreach my $stat (@summaries) {
        open STAT, $stat;
        my @row = split(/\D+/, <STAT>);
        $stats[$_] += $row[$_ + 1] for (0 .. ($#row - 1));
        close STAT
      }

      my $total = $stats[0] + $stats[1];
      my $skip = $stats[2] + $stats[3];
      $client->log("$stats[0] out of $total tests succeeded ($skip skipped)");

      if ($stats[1] > 0 || $stats[0] < $total) {
        $client->log("$stats[1] tests failed.");
        open TOUCH_FAILED, ">>$out/nix-support/failed";
        close TOUCH_FAILED;
      }
    });
  '';
}
