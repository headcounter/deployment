-module(testclient).
-compile(export_all).
-behaviour(gen_server).

-record(state, {config, users = []}).

-include_lib("escalus/include/escalus.hrl").

start() ->
    gen_server:start({local, testclient}, ?MODULE, none, []).

config() -> [
    {escalus_user_db, xmpp},
    {escalus_host, <<"server">>},
    {escalus_server, <<"server">>},
    {escalus_users, [
        {user1, [{username, <<"user1">>},
                 {password, <<"testuser1">>}]},
        {user2, [{username, <<"user2">>},
                 {password, <<"testuser2">>}]}
    ]}
].

init(_) ->
    {ok, _} = application:ensure_all_started(escalus),
    application:set_env(escalus, common_test, false),
    Config = escalus_event:start(escalus_cleaner:start(config())),
    {ok, #state{config = Config}}.

handle_call(ping, _, State) ->
    {reply, pong, State};

handle_call(register, _, #state{config = Config} = State) ->
    Users = escalus_config:get_config(escalus_users, Config),
    Created = [escalus_users:create_user(Config, User) || User <- Users],
    lists:foreach(fun escalus_users:verify_creation/1, Created),
    NewConf = lists:keystore(escalus_users, 1, Config, {escalus_users, Users}),
    {reply, register_done, State#state{config = NewConf}};

handle_call(login, _, #state{config = Config} = State) ->
    Users = escalus_config:get_config(escalus_users, Config),
    Login = fun(Name) ->
        UserSpec = escalus_users:get_options(Config, Name),
        {Name, escalus_client:start(Config, UserSpec, <<"test">>)}
    end,
    ConnectedUsers = lists:map(Login, proplists:get_keys(Users)),
    CheckLoggedIn = fun(C) ->
        Message = <<"Am I logged in?">>,
        OwnJID = escalus_client:full_jid(C),
        Stanza = escalus_stanza:chat_to(OwnJID, Message),
        ok = escalus_client:send(C, Stanza),
        try escalus_client:wait_for_stanza(C, 10000) of
            Result -> escalus_pred:is_chat_message(Message, Result)
        catch
            _:_ -> false
        end
    end,
    CheckConnection = fun({_, {ok, C}})    -> CheckLoggedIn(C);
                         ({_, {error, _}}) -> false
                      end,
    Status = case lists:all(CheckConnection, ConnectedUsers) of
        true  -> logged_in;
        false -> login_failure
    end,
    OnlyConnected = fun({Name, {ok, C}}) -> {true, {Name, C}};
                       ({_, {error, _}}) -> false
                    end,
    Extracted = lists:filtermap(OnlyConnected, ConnectedUsers),
    {reply, Status, State#state{users = Extracted}};

handle_call(communicate, _, #state{users = Users} = State) ->
    User1 = proplists:get_value(user1, Users),
    User2 = proplists:get_value(user2, Users),

    ChatTo = fun(From, To) ->
        JidTo = escalus_client:full_jid(To),
        Username = escalus_client:username(From),
        Message = <<"Hello from ", Username/binary>>,
        Stanza = escalus_stanza:chat_to(JidTo, Message),
        ok = escalus_client:send(User1, Stanza),
        Result = escalus_client:wait_for_stanza(To, 60000),
        true = escalus_pred:is_chat_message(Message, Result),
        ok
    end,

    ok = ChatTo(User1, User2),
    ok = ChatTo(User2, User1),

    {reply, great_communication, State};

handle_call(adhoc_ping, _, #state{users = Users} = State) ->
    User = proplists:get_value(user1, Users),
    Msg = <<"ping">>,
    PingStanza = escalus_stanza:adhoc_request(Msg),
    Stanza = escalus_stanza:to(PingStanza, <<"server">>),
    ok = escalus_client:send(User, Stanza),
    Response = escalus_client:wait_for_stanza(User, 60000),
    true = escalus_pred:is_adhoc_response(Msg, <<"completed">>, Response),
    Path = [{element, <<"command">>}, {element, <<"note">>}, cdata],
    Result = case exml_query:path(Response, Path) of
        <<"Pong">> -> pong;
        <<"Pang">> -> pang
    end,
    {reply, Result, State};

handle_call(check_connections, _, #state{users = Users} = State) ->
    IsConnected = fun(U) ->
        escalus_connection:is_connected(element(2, U))
    end,
    Result = case lists:all(IsConnected, Users) of
        true  -> still_connected;
        false -> not_connected_anymore
    end,
    {reply, Result, State};

handle_call(get_uptime, _, #state{users = Users} = State) ->
    User1 = proplists:get_value(user1, Users),
    ok = escalus_client:send(User1, escalus_stanza:last_activity(<<"server">>)),
    Reply = escalus_client:wait_for_stanza(User1, 60000),
    Result = exml_query:path(Reply, [
        {element, <<"query">>},
        {attr, <<"seconds">>}
    ]),
    {reply, list_to_integer(binary_to_list(Result)), State};

handle_call(_, _, State) ->
    {reply, wrong_call, State}.

terminate(_, #state{config = Config}) ->
    escalus_cleaner:clean(Config).

handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
code_change(_, State, _) -> {ok, State}.
