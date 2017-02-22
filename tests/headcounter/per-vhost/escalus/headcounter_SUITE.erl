-module(headcounter_SUITE).
-compile(export_all).

-include_lib("escalus/include/escalus.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [{group, headcounter}].

groups() ->
    [{headcounter, [sequence], [ensure_inband_reg_is_disabled]}].

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
