-module(headcounter_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("escalus/include/escalus_xmlns.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("exml/include/exml_stream.hrl").

all() -> [{group, G} || G <- enabled_groups()].

enabled_groups() ->
    [headcounter | per_vhost_groups(os:getenv("FQDN"))].

groups() ->
    [{headcounter, [sequence], [ensure_inband_reg_is_disabled,
                                mam_no_store_by_default,
                                mam_store_is_working]},
     {torservers, [sequence], [ensure_own_muc_service]}].

-spec per_vhost_groups(string()) -> [atom()].
per_vhost_groups("torservers.net") ->
    [torservers];
per_vhost_groups(_) ->
    [].

suite() ->
    escalus:suite().

init_per_suite(Config) ->
    escalus:init_per_suite(Config).

end_per_suite(Config) ->
    escalus:end_per_suite(Config).

init_per_testcase(CaseName, Config) ->
    escalus:init_per_testcase(CaseName, Config).

end_per_testcase(CaseName, Config) ->
    escalus:end_per_testcase(CaseName, Config).

ensure_inband_reg_is_disabled(Config) ->
    [User] = escalus_users:get_users([outsider]),
    {error, failed_to_register, R} = escalus_users:create_user(Config, User),
    escalus:assert(is_iq_error, R),
    escalus:assert(is_error, [<<"auth">>, <<"forbidden">>], R).

%% Xep-0313 archived messages, copied from newer escalus version.
%% TODO: Remove both functions as soon as we're on a newer version of escalus.
-spec is_mam_archived_message(binary(), exml:element()) ->
    boolean().
is_mam_archived_message(Msg, #xmlel{} = Stanza) ->
    M = exml_query:path(Stanza, [{element, <<"result">>},
                                 {element, <<"forwarded">>},
                                 {element, <<"message">>}]),
    escalus_pred:is_chat_message(Msg, M).

-spec is_mam_fin_message(exml:element()) -> boolean().
is_mam_fin_message(Stanza) ->
    case exml_query:path(Stanza, [{element, <<"fin">>}]) of
        undefined  ->
            false;
        FinEl ->
            exml_query:attr(FinEl, <<"xmlns">>) == <<"urn:xmpp:mam:1">>
    end.

-spec chitchat(term(), term()) -> [binary()].
chitchat(Alice, Bob) ->
    Msg1 = <<"En Taro Adun">>,
    Msg2 = <<"Ner'mah">>,
    escalus:send(Alice, escalus_stanza:chat_to(Bob, Msg1)),
    escalus:assert(is_chat_message, [Msg1],
                   escalus:wait_for_stanza(Bob)),
    escalus:send(Bob, escalus_stanza:chat_to(Alice, Msg2)),
    escalus:assert(is_chat_message, [Msg2],
                   escalus:wait_for_stanza(Alice)),
    [Msg1, Msg2].

mam_no_store_by_default(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        chitchat(Alice, Bob),

        escalus:send(Alice, escalus_stanza:iq(<<"set">>, [#xmlel{
            name = <<"query">>,
            attrs = [{<<"xmlns">>, <<"urn:xmpp:mam:1">>},
                     {<<"queryid">>, <<"q1">>}]
        }])),
        Fin = escalus:wait_for_stanza(Alice),
        escalus:assert(fun ?MODULE:is_mam_fin_message/1, Fin),
        ok
    end).

mam_store_is_working(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        AlwaysStoreRequest = escalus_stanza:iq(<<"set">>, [#xmlel{
            name = <<"prefs">>,
            attrs = [{<<"xmlns">>, <<"urn:xmpp:mam:1">>},
                     {<<"default">>, <<"always">>}],
            children = [#xmlel{name = <<"always">>}, #xmlel{name = <<"never">>}]
        }]),
        escalus:send(Alice, AlwaysStoreRequest),
        Response = escalus:wait_for_stanza(Alice),
        escalus:assert(is_iq_result, Response),

        [Msg1, Msg2] = chitchat(Alice, Bob),

        escalus:send(Alice, escalus_stanza:iq(<<"set">>, [#xmlel{
            name = <<"query">>,
            attrs = [{<<"xmlns">>, <<"urn:xmpp:mam:1">>},
                     {<<"queryid">>, <<"q1">>}]
        }])),
        [M1, M2, M3] = escalus:wait_for_stanzas(Alice, 3),
        escalus:assert(fun ?MODULE:is_mam_archived_message/2, [Msg1], M1),
        escalus:assert(fun ?MODULE:is_mam_archived_message/2, [Msg2], M2),
        escalus:assert(fun ?MODULE:is_mam_fin_message/1, M3),
        ok
    end).

ensure_own_muc_service(Config) ->
    escalus:story(Config, [{alice, 1}, {bob, 1}], fun(Alice, Bob) ->
        Server = escalus_client:server(Alice),
        escalus:send(Alice, escalus_stanza:service_discovery(Server)),
        Response = escalus:wait_for_stanza(Alice),
        MucHost = <<"conference.", Server/binary>>,
        escalus:assert(has_service, [MucHost], Response),

        Presence = escalus_stanza:presence(<<"available">>, [#xmlel{
            name = <<"x">>,
            attrs = [{<<"xmlns">>, <<"http://jabber.org/protocol/muc">>}]
        }]),
        Room = <<"test@", MucHost/binary>>,

        RoomAlice = escalus_stanza:to(Presence, <<Room/binary, "/alice">>),
        RoomBob = escalus_stanza:to(Presence, <<Room/binary, "/bob">>),

        escalus:send(Alice, RoomAlice),
        escalus:wait_for_stanza(Alice),

        NoConfig = escalus_stanza:iq_set(?NS_MUC_OWNER, [#xmlel{
            name = <<"x">>,
            attrs = [{<<"xmlns">>, ?NS_DATA_FORMS},
                     {<<"from">>, escalus_utils:get_jid(Alice)},
                     {<<"type">>, <<"submit">>}]
        }]),
        escalus:send(Alice, escalus_stanza:to(NoConfig, Room)),

        escalus:wait_for_stanzas(Alice, 2),
        escalus:send(Bob, RoomBob),
        % Skip presence of bob
        escalus:wait_for_stanza(Alice),

        escalus:wait_for_stanzas(Bob, 3),

        Msg = <<"Hello bob!">>,
        escalus:send(Alice, escalus_stanza:groupchat_to(Room, Msg)),

        Received = escalus:wait_for_stanza(Bob),
        escalus:assert(is_groupchat_message, [Msg], Received),
        ok
    end).
