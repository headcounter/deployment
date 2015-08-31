-spec get_app_trylist(atom()) -> [file:filename_all()].
get_app_trylist(AppName) ->
    NameStr = atom_to_list(AppName),
    [ filename:join("src", NameStr ++ ".app.src")
    , filename:join("src", NameStr ++ ".app")
    , filename:join("ebin", NameStr ++ ".app")
    ].

-spec get_app_file(file:name(), atom()) -> {ok, file:name()} | not_found.
get_app_file(Basedir, AppName) ->
    ToTry = [filename:join(Basedir, Try) || Try <- get_app_trylist(AppName)],
    case lists:filter(fun filelib:is_file/1, ToTry) of
        [First|_] -> {ok, First};
        _         -> not_found
    end.

-spec get_app_file(file:name()) -> file:name().
get_app_file(Basedir) ->
    AppName = case filename:basename(Basedir) of
        Base when is_list(Base) -> list_to_atom(Base);
        Base when is_binary(Base) -> binary_to_atom(Base, unicode)
    end,
    get_app_file(Basedir, AppName).

-spec get_appfiles(string()) -> [file:filename_all()].
get_appfiles(Name) ->
    SubAppFiles = case file:list_dir("apps") of
        {error, _} -> [];
        {ok, Files} ->
            Dirs = [F || F <- Files, filelib:is_dir(filename:join("apps", F))],
            AppFiles = [get_app_file(filename:join("apps", D)) || D <- Dirs],
            Filter = fun({ok, File}) -> {true, File};
                        (_)          -> false
            end,
            lists:filtermap(Filter, AppFiles)
    end,

    MainAppFiles = case get_app_file(".", list_to_atom(Name)) of
        {ok, AppFile} -> [AppFile];
        _             -> []
    end,

    MainAppFiles ++ SubAppFiles.

-spec rewrite_version(Vsn :: string(), Replacement :: string()) -> string().
rewrite_version(_, Replacement) -> Replacement.

-spec rewrite_appver(file:filename_all(), string()) -> ok | {error, atom()}.
rewrite_appver(File, Hash) ->
    {ok, [{application, AppName, Keys}]} = file:consult(File),
    {vsn, OrigVsn} = lists:keyfind(vsn, 1, Keys),
    NewVsn = rewrite_version(OrigVsn, Hash),
    NewKeys = lists:keyreplace(vsn, 1, Keys, {vsn, NewVsn}),
    NewData = {application, AppName, NewKeys},
    Result = file:write_file(File, [io_lib:format("~tp.~n", [NewData])]),
    Msg = "Rewrote ~s from version ~tp to ~tp.~n",
    io:fwrite(standard_error, Msg, [File, OrigVsn, NewVsn]),
    Result.

-spec do_strip_comments(binary(), binary()) -> binary().
do_strip_comments(<<$%, Rest/binary>>, Acc) ->
    case binary:split(Rest, <<$\n>>) of
        [_, NextLines] -> do_strip_comments(NextLines, Acc);
        _              -> do_strip_comments(Rest, <<Acc/binary, $%>>)
    end;
do_strip_comments(<<X, Rest/binary>>, Acc) ->
    do_strip_comments(Rest, <<Acc/binary, X>>);
do_strip_comments(<<>>, Acc) ->
    Acc.

-spec strip_comments(file:filename_all()) -> file:filename_all().
strip_comments(File) ->
    case file:read_file(File) of
        {ok, Data} ->
            Stripped = do_strip_comments(Data, <<>>),
            file:write_file(File, Stripped),
            File;
        _ ->
            File
    end.

-spec err_out(string()) -> no_return().
err_out(Msg) ->
    io:fwrite(standard_error, "~s~n", [Msg]),
    halt(1).

main([Hash, Name]) ->
    AppFiles = lists:map(fun strip_comments/1,  get_appfiles(Name)),
    Rewritten = case [rewrite_appver(F, Hash) || F <- AppFiles] of
        [] -> err_out("Cannot find .app files to rewrite!");
        AF -> AF
    end,
    case lists:all(fun(ok) -> true; (_) -> false end, Rewritten) of
        false -> err_out("Error rewriting application files!");
        true  -> halt(0)
    end.
