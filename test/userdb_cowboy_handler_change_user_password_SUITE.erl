-module(userdb_cowboy_handler_change_user_password_SUITE).

-compile({parse_transform, lager_transform}).

-include("include/userdb_mysql_manager_api.hrl").

%% API
-export([
    all/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    not_found_session_id/1,
    unauthorized/1,
    change_user_password_timeout/1,
    bad_password_or_new_password/1,
    error_of_change_user_password/1,
    user_password_changed_successfully/1,
    wrong_password/1
]).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() -> [
    not_found_session_id,
    unauthorized,
    change_user_password_timeout,
    bad_password_or_new_password,
    error_of_change_user_password,
    user_password_changed_successfully,
    wrong_password
].

suite() ->
    [{timetrap, {seconds, 5}}].

init_per_suite(Config) ->
    userdb_cowboy_handler_registration_SUITE:create_database_and_table(),
    userdb:lager(),
    userdb:start(),
    Config.

end_per_suite(_Config) ->
    userdb_cowboy_handler_registration_SUITE:drop_database(),
    userdb:stop(),
    userdb:nolager(),
    ok.

init_per_testcase(change_user_password_timeout, Config) ->
    inets:start(),
    meck:new(userdb_mysql_manager, [unstick, passthrough]),
    meck:expect(
        userdb_mysql_manager,
        cast,
        fun
            (#change_user_password_request{} = _) ->
                erlang:make_ref();
            (Body) ->
                meck:passthrough([Body])
        end
    ),
    Config;
init_per_testcase(error_of_change_user_password, Config) ->
    inets:start(),
    Config;
init_per_testcase(_TestCase, Config) ->
    inets:start(),
    Config.

end_per_testcase(change_user_password_timeout, _Config) ->
    meck:unload(userdb_mysql_manager),
    inets:stop(),
    ok;
end_per_testcase(error_of_change_user_password, _Config) ->
    meck:unload(mysql),
    inets:stop(),
    ok;
end_per_testcase(_TestCase, _Config) ->
    inets:stop(),
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================
not_found_session_id(_Config) ->
    Data = <<"{\"password\":\"user password\",\"new_password\":\"new user password\"}">>,
    ?assertEqual(
        {ok, {400, "{\"description\":\"Not found session_id in cookie\"}"}},
        httpc:request(post, {"http://localhost:8080/change_user_password", [], "application/json", Data}, [], [{full_result, false}])
    ).

unauthorized(_Config) ->
    Data = <<"{\"password\":\"user password\",\"new_password\":\"new user password\"}">>,
    ?assertEqual(
        {ok, {400, "{\"description\":\"Unauthorized\"}"}},
        httpc:request(post, {"http://localhost:8080/change_user_password", [{"cookie", "session_id=bad session id"}], "application/json", Data}, [], [{full_result, false}])
    ).

bad_password_or_new_password(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    {ok, _} = httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}]),
    {ok, {{_, 200, _}, Headers, _}} = httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data}, [], []),
    {_, Cookie} = lists:keyfind("set-cookie", 1, Headers),
    Data2 = <<"{\"password\":\"\",\"new_password\":1234}">>,
    ?assertEqual(
        {ok, {400, "{\"description\":\"Bad password or new password\"}"}},
        httpc:request(post, {"http://localhost:8080/change_user_password", [{"cookie", Cookie}], "application/json", Data2}, [], [{full_result, false}])
    ).

change_user_password_timeout(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    {ok, _} = httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}]),
    {ok, {{_, 200, _}, Headers, _}} = httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data}, [], []),
    {_, Cookie} = lists:keyfind("set-cookie", 1, Headers),
    Data2 = <<"{\"password\":\"user password\",\"new_password\":\"new user password\"}">>,
    ?assertEqual(
        {ok, {500, "{\"description\":\"Change user password timeout\"}"}},
        httpc:request(post, {"http://localhost:8080/change_user_password", [{"cookie", Cookie}], "application/json", Data2}, [], [{full_result, false}])
    ).

error_of_change_user_password(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    {ok, _} = httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}]),
    {ok, {{_, 200, _}, Headers, _}} = httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data}, [], []),
    {_, Cookie} = lists:keyfind("set-cookie", 1, Headers),
    Data2 = <<"{\"password\":\"user password\",\"new_password\":\"new user password\"}">>,
    meck:new(mysql),
    meck:expect(mysql, query, fun (_, _) -> {error, unsuccess_request} end),
    ?assertEqual(
        {ok, {400, "{\"description\":\"Error of change user password\"}"}},
        httpc:request(post, {"http://localhost:8080/change_user_password", [{"cookie", Cookie}], "application/json", Data2}, [], [{full_result, false}])
    ).

user_password_changed_successfully(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    {ok, _} = httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}]),
    {ok, {{_, 200, _}, Headers, _}} = httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data}, [], []),
    {_, Cookie} = lists:keyfind("set-cookie", 1, Headers),
    Data2 = <<"{\"password\":\"user password\",\"new_password\":\"new user password\"}">>,
    ?assertEqual(
        {ok,{200,"{\"description\":\"User password changed successfully\"}"}},
        httpc:request(post, {"http://localhost:8080/change_user_password", [{"cookie", Cookie}], "application/json", Data2}, [], [{full_result, false}])
    ),
    Data3 = <<"{\"user\":\"it is user\",\"password\":\"new user password\"}">>,
    ?assertEqual(
        {ok, {200, "{\"description\":\"Success authorization\"}"}},
        httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data3}, [], [{full_result, false}])
    ).

wrong_password(_Config) ->
    Data = <<"{\"user\":\"it is user 2\",\"password\":\"user password\"}">>,
    {ok, _} = httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}]),
    {ok, {{_, 200, _}, Headers, _}} = httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data}, [], []),
    {_, Cookie} = lists:keyfind("set-cookie", 1, Headers),
    Data2 = <<"{\"password\":\"wrong user password\",\"new_password\":\"new user password\"}">>,
    ?assertEqual(
        {ok,{400,"{\"description\":\"Wrong user password\"}"}},
        httpc:request(post, {"http://localhost:8080/change_user_password", [{"cookie", Cookie}], "application/json", Data2}, [], [{full_result, false}])
    ).
