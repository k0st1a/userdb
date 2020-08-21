-module(userdb_cowboy_handler_get_users_list_SUITE).

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
    get_users_list_timeout/1,
    bad_offset_or_limit/1,
    unsuccess_get_users_list_request/1,
    success_get_users_list_request/1
]).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() -> [
    not_found_session_id,
    unauthorized,
    get_users_list_timeout,
    bad_offset_or_limit,
    unsuccess_get_users_list_request,
    success_get_users_list_request
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

init_per_testcase(get_users_list_timeout, Config) ->
    inets:start(),
    meck:new(userdb_mysql_manager, [unstick, passthrough]),
    meck:expect(
        userdb_mysql_manager,
        cast,
        fun
            (#get_users_list_request{} = _) ->
                erlang:make_ref();
            (Body) ->
                meck:passthrough([Body])
        end
    ),
    Config;
init_per_testcase(unsuccess_get_users_list_request, Config) ->
    inets:start(),
    Config;
init_per_testcase(_TestCase, Config) ->
    inets:start(),
    Config.

end_per_testcase(get_users_list_timeout, _Config) ->
    meck:unload(userdb_mysql_manager),
    inets:stop(),
    ok;
end_per_testcase(unsuccess_get_users_list_request, _Config) ->
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
    ?assertEqual(
        {ok, {400, "{\"description\":\"Not found session_id in cookie\"}"}},
        httpc:request(get, {"http://localhost:8080/get_users_list", []}, [], [{full_result, false}])
    ).

unauthorized(_Config) ->
    ?assertEqual(
        {ok, {400, "{\"description\":\"Unauthorized\"}"}},
        httpc:request(get, {"http://localhost:8080/get_users_list", [{"cookie", "session_id=bad session id"}]}, [], [{full_result, false}])
    ).

get_users_list_timeout(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    {ok, _} = httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}]),
    {ok, {{_, 200, _}, Headers, _}} = httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data}, [], []),
    {_, Cookie} = lists:keyfind("set-cookie", 1, Headers),
    Data2 = <<"{\"offset\":0,\"limit\":50}">>,
    ?assertEqual(
        {ok, {400, "{\"description\":\"Get users list timeout\"}"}},
        httpc:request(post, {"http://localhost:8080/get_users_list", [{"cookie", Cookie}], "application/json", Data2}, [], [{full_result, false}])
    ).

bad_offset_or_limit(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    {ok, _} = httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}]),
    {ok, {{_, 200, _}, Headers, _}} = httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data}, [], []),
    {_, Cookie} = lists:keyfind("set-cookie", 1, Headers),
    Data2 = <<"{\"offset\":-120,\"limit\":100}">>,
    ?assertEqual(
        {ok, {400, "{\"description\":\"Bad offset or limit\"}"}},
        httpc:request(post, {"http://localhost:8080/get_users_list", [{"cookie", Cookie}], "application/json", Data2}, [], [{full_result, false}])
    ),
    Data3 = <<"{\"offset\":100,\"limit\":-100}">>,
    ?assertEqual(
        {ok, {400, "{\"description\":\"Bad offset or limit\"}"}},
        httpc:request(post, {"http://localhost:8080/get_users_list", [{"cookie", Cookie}], "application/json", Data3}, [], [{full_result, false}])
    ).

unsuccess_get_users_list_request(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    {ok, _} = httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}]),
    {ok, {{_, 200, _}, Headers, _}} = httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data}, [], []),
    {_, Cookie} = lists:keyfind("set-cookie", 1, Headers),
    Data2 = <<"{\"offset\":0,\"limit\":50}">>,
    meck:new(mysql),
    meck:expect(mysql, query, fun (_, _) -> {error, unsuccess_request} end),
    ?assertEqual(
        {ok, {400, "{\"description\":\"Unsuccess get users list request\"}"}},
        httpc:request(post, {"http://localhost:8080/get_users_list", [{"cookie", Cookie}], "application/json", Data2}, [], [{full_result, false}])
    ).

success_get_users_list_request(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    {ok, _} = httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}]),
    {ok, {{_, 200, _}, Headers, _}} = httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data}, [], []),
    {_, Cookie} = lists:keyfind("set-cookie", 1, Headers),
    Data2 = <<"{\"offset\":0,\"limit\":50}">>,
    ?assertEqual(
        {ok,{200,"{\"list\":\"it is user\"}"}},
        httpc:request(post, {"http://localhost:8080/get_users_list", [{"cookie", Cookie}], "application/json", Data2}, [], [{full_result, false}])
    ).
