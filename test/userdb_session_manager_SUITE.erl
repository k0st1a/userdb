-module(userdb_session_manager_SUITE).

-compile({parse_transform, lager_transform}).

-include("userdb_session_manager_api.hrl").
-include("userdb_session.hrl").

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
    remove_expired_sessions/1
]).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() -> [
    remove_expired_sessions
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

init_per_testcase(_TestCase, Config) ->
    inets:start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
    inets:stop(),
    ok.

%%%===================================================================
%%% Test cases
%%%===================================================================
remove_expired_sessions(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    {ok, {200, _}} = httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}]),
    {ok, {200, _}} = httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data}, [], [{full_result, false}]),
    ?assertMatch(
        [
            #session{user = <<"it is user">>}
        ],
        userdb_session_manager:find(#session_filter{})
    ),
    Data2 = <<"{\"user\":\"it is user '2\",\"password\":\"user password 2\"}">>,
    {ok, {200, _}} = httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data2}, [], [{full_result, false}]),
    {ok, {200, _}} = httpc:request(post, {"http://localhost:8080/authorization", [], "application/json", Data2}, [], [{full_result, false}]),
    ?assertMatch(
        [
            #session{user = <<"it is user">>},
            #session{user = <<"it is user '2">>}
        ],
        userdb_session_manager:find(#session_filter{})
    ),
    {ok, TTL} = application:get_env(userdb, session_ttl),
    timer:sleep(TTL),
    ?assertMatch(
        [
            #session{user = <<"it is user '2">>}
        ],
        userdb_session_manager:find(#session_filter{})
    ),
    timer:sleep(TTL),
    ?assertEqual(
        [],
        userdb_session_manager:find(#session_filter{})
    ).
