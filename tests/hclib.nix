{ runCommand, writeScript, nix, erlang }:

{
  test = runCommand "test-run-hclib" {
    buildInputs = [ nix erlang ];
    testAddresses = [
      "-1.2.3.4" "10." "172.16." "198.168.0." "127.0.0.1." ":::" "f:::2"
      "0.0.0.0" "1.2.3.4" "253.252.251.250" "1.2.255.254" "::" "f::2" "::-1"
      "0.00.0.0" "0.0.000000000000.0" "0.256.0.1" "1.2.3.4.5" "256.255.65535"
      "00000000000037777777777" "0xffffffff" "0x12345678" "0x12.0x345678"
      "01000::" "::8:7:6:5:4:3:2:1" "8:7:6:5:4:3:2:1::" "8:7:6:5:4::3:2:1"
      "0x12.0X34.0x5678" "0x12.0X34.0x56.0X78" "255.0xFF.0177777" "4294967295"
      "255.255.255.0377" "255.16777215" "00377.0XFFFFFF" "255.255.65535"
      "4294967296" "0Xff.000000000377.0x0000ff.255" "0x100000000" "7:6:5:4:3::"
      "040000000000" "1.2.3.-4" "1.2.-3.4" "1.-2.3.4" "7:6:5:4::"
      "7:6:5:4:3:2::" "7:6:5:4:3:2:1::" "::1.256.3.4" "::-5.4.3.2" "::5.-4.3.2"
      "::5.4.-3.2" "::5.4.3.-2" "::FFFF:1.2.3.4.5" "::10." "::FFFF:172.16."
      "::5:4:3:2:1" "::6:5:4:3:2:1" "::7:6:5:4:3:2:1" "7::" "7:6::" "7:6:5::"
      "::g" "f:f11::10100:2" "f:f11::01100:2" "::17000" "::01700" "10000::"
      "c11:0c22:5c33::0088" "c11:0c22::0088" "::FFFF:1.2.255.254"
      "c11:0c22:5c33::55c0:c66c:77:0088" "c11:0c22:5c33:c440::c66c:77:0088"
      "c11:0c22:5c33::c66c:77:0088" "c11:0c22:5c33:c440::77:0088"
      "c11:0c22:5c33:c440:55c0::0088" "c11::55c0:c66c:77:0088"
      "c11:0c22:5c33:c440:55c0::77:0088" "c11:0c22:5c33:c440:55c0:c66c::0088"
      "c11:0c22:5c33:c440::0088" "c11::c66c:77:0088" "c11:0c22::77:0088"
      "c11:0c22::c66c:77:0088" "c11:0c22:5c33::77:0088"
      "c11:0c22:5c33:c440:55c0:c66c:77:0088" "c11::5c33:c440:55c0:c66c:77:0088"
      "c11:0c22::c440:55c0:c66c:77:0088" "c11::c440:55c0:c66c:77:0088"
      "c11:0c22::55c0:c66c:77:0088" "c11::77:0088" "f:f11::0100:2" "::17"
      "0700::" "::2:1" "::3:2:1" "::4:3:2:1" "fe80::198.168.0."
      "fec0::fFfF:127.0.0.1." "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"
      "10.0x019876" "012.01.054321" "0" "00" "0.0" "00.00.00" "::1.2.3.4.5"
      "::1.2.3.04"
    ];

    testParseIp = writeScript "test-parse-ip.erl" ''
      -module(test_parse_ip).
      -export([main/0]).

      -spec main() -> no_return().
      main() ->
        Ips = init:get_plain_arguments(),
        Result = lists:map(fun(Ip) ->
          NixTest = case nix_test(Ip) of
            {ok, Result} ->
              Stripped = re:replace(Result, "^\"|[\n\"]$", "",
                                    [global, {return, binary}]),
              Code = binary_to_list(<<Stripped/binary, $.>>),
              case erl_scan:string(Code) of
                {ok, Tokens, _} ->
                  {ok, [Form]} = erl_parse:parse_exprs(Tokens),
                  {value, Value, _} = erl_eval:expr(Form, []),
                  {ok, Value};
                {error, _, _} -> {ok, Result}
              end;
            Rest -> Rest
          end,
          case {NixTest, inet:parse_strict_address(Ip)} of
            {{ok, Res}, {ok, Res}}  ->
              io:format("Test for IP ~p passed (same value).~n", [Ip]),
              true;
            {{fail, _}, {error, _}} ->
              io:format("Test for IP ~p passed (error on both).~n", [Ip]),
              true;
            {NixRes, ErlRes} ->
              io:format("FAILED test for IP ~p!~n" ++
                        "  Nix expression returned: ~p~n" ++
                        "  Erlang returned: ~p~n", [Ip, NixRes, ErlRes]),
              false
          end
        end, Ips),
        LazyResult = lists:all(fun(X) -> X end, Result),
        erlang:halt(case LazyResult of true -> 0; _ -> 1 end).

      -spec gather_result(port(), binary()) -> {ok | fail, binary()}.
      gather_result(Port, Partial) ->
        receive
          {Port, {data, Data}} ->
            gather_result(Port, <<Partial/binary, Data/binary>>);
          {Port, eof} -> Port ! {self(), close}, gather_result(Port, Partial);
          {Port, {exit_status, 0}} -> {ok, Partial};
          {Port, {exit_status, _}} -> {fail, Partial};
          _ -> gather_result(Port, Partial)
        after 1 ->
          gather_result(Port, Partial)
        end.

      -spec nix_test(string()) -> {ok | fail, binary()}.
      nix_test(Ip) ->
        TestLib = "(import <testlib> { lib = import <nixpkgslib>; })",
        Command = "parseErlIpAddr",
        Code = TestLib ++ "." ++ Command ++ " " ++ "\"" ++ Ip ++ "\"",
        Args = [
          "-I", "testlib=" ++ "${../lib}",
          "-I", "nixpkgslib=" ++ "${<nixpkgs/lib>}",
          "--readonly-mode", "--eval",
          "-E", Code
        ],
        Port = open_port(
          {spawn_executable, "${nix}/bin/nix-instantiate"},
          [{args, Args}, binary, in, stream, exit_status, use_stdio,
           stderr_to_stdout]
        ),
        gather_result(Port, <<>>).
    '';

  } ''
    export NIX_DB_DIR=$TMPDIR
    export NIX_STATE_DIR=$TMPDIR
    nix-store --init

    cat "$testParseIp" > test_parse_ip.erl
    erlc -Wall test_parse_ip.erl

    erl -smp -noinput -s test_parse_ip main -extra $testAddresses
    touch "$out"
  '';
}
