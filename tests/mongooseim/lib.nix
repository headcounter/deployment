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
        lists:member({Host, Port}, AllowedHostPort) or
            lists:member(Port, AllowedPorts)
    end,
    IsListening = fun(Port) ->
        {ok, Status} = prim_inet:getstatus(Port),
        lists:member(listen, Status)
    end,
    Recurse = fun
        (_, Pid, 5) -> Pid;
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
}
