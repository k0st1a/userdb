-module(userdb_cowboy_handler_authorization_SUITE).

-compile({parse_transform, lager_transform}).

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
    bad_authorization_request/1,
    bad_user_or_password/1,
    unsuccess_authorization/1,
    success_authorization/1,
    authorization_timeout/1,
    make_session_timeout/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() -> [
    bad_authorization_request,
    bad_user_or_password,
    unsuccess_authorization,
    success_authorization,
    authorization_timeout,
    make_session_timeout
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

init_per_testcase(unsuccess_authorization, Config) ->
    inets:start(),
    meck:new(mysql),
    meck:expect(mysql, query, fun (_, _) -> {error, unsuccess_request} end),
    Config;
init_per_testcase(authorization_timeout, Config) ->
    inets:start(),
    meck:new(userdb_mysql_manager),
    meck:expect(userdb_mysql_manager, cast, fun (_) -> erlang:make_ref() end),
    Config;
init_per_testcase(make_session_timeout, Config) ->
    inets:start(),
    meck:new(userdb_session_manager),
    meck:expect(userdb_session_manager, cast, fun (_) -> erlang:make_ref() end),
    Config;
init_per_testcase(_TestCase, Config) ->
    inets:start(),
    Config.

end_per_testcase(unsuccess_authorization, _Config) ->
    meck:unload(mysql),
    inets:stop(),
    ok;
end_per_testcase(authorization_timeout, _Config) ->
    meck:unload(userdb_mysql_manager),
    inets:stop(),
    ok;
end_per_testcase(make_session_timeout, _Config) ->
    meck:unload(userdb_session_manager),
    inets:stop(),
    ok;
end_per_testcase(_TestCase, _Config) ->
    inets:stop(),
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================
bad_authorization_request(_Config) ->
    ?assertEqual(
        {ok, {400, "{\"description\":\"Bad authorization request\"}"}},
        httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", <<>>}, [], [{full_result, false}])
    ).

bad_user_or_password(_Config) ->
    Data = <<"{\"user\":\"it is user not registered\",\"password\":\"user password\"}">>,
    ?assertEqual(
        {ok,{400,"{\"description\":\"Bad user or password\"}"}},
        httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data}, [], [{full_result, false}])
    ),
    Data2 = <<"{\"user\":\"it is user\",\"password\":\"bad user password\"}">>,
    ?assertEqual(
        {ok,{400,"{\"description\":\"Bad user or password\"}"}},
        httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data2}, [], [{full_result, false}])
    ).

unsuccess_authorization(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    ?assertEqual(
        {ok,{400,"{\"description\":\"Unsuccess authorization\"}"}},
        httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data}, [], [{full_result, false}])
    ).

success_authorization(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    {ok, {200, _}} = httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}]),
    {ok, Result} = httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data}, [], []),
    {StatusLine, Headers, Body} = Result,
    ?assertMatch({_, 200, _}, StatusLine),
    ?assertEqual("{\"description\":\"Success authorization\"}", Body),
    ?assertMatch({_, "session_id=" ++ _}, lists:keyfind("set-cookie", 1, Headers)).

authorization_timeout(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    ?assertEqual(
        {ok, {408, "{\"description\":\"Authorization timeout\"}"}},
        httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data}, [], [{full_result, false}])
    ).

make_session_timeout(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    ?assertEqual(
        {ok, {408, "{\"description\":\"Make session timeout\"}"}},
        httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data}, [], [{full_result, false}])
    ).
