-module('rewrite-rebar-config').
-export([main/1]).

do_rewrite(File) ->
    try
        {ok, Original} = file:consult(File),
        {deps, OrigDeps} = lists:keyfind(deps, 1, Original),
        NewDeps = [{Name, ".*", Src} || {Name, _, Src} <- OrigDeps],
        Rewritten = lists:keyreplace(deps, 1, Original, {deps, NewDeps}),
        PPrint = fun(T) ->
            io_lib:format("~tp.~n", [T])
        end,
        Data = lists:map(PPrint, Rewritten),
        file:write_file(File, Data)
    catch
        _:_ -> {error, ignored}
    end.

main(Files) ->
    lists:map(fun do_rewrite/1, Files).
