-module(userdb_cowboy_handler_registration_SUITE).

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
    registration/1,
    registration_timeout/1,
    registration_bad/1
]).

%% For other tests
-export([
    create_database_and_table/0,
    drop_database/0
]).


-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("userdb_mysql_manager_api.hrl").
-include("userdb_msg.hrl").

-define(CONSOLE(Format, Args), io:format(user, "[~100p:~100p] " ++ Format, [?MODULE, ?LINE] ++ Args)).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() -> [
    registration,
    registration_timeout,
    registration_bad
].

suite() ->
    [{timetrap, {seconds, 5}}].

init_per_suite(Config) ->
    create_database_and_table(),
    %userdb:lager(),
    userdb:start(),
    Config.

end_per_suite(_Config) ->
    drop_database(),
    userdb:stop(),
    %userdb:nolager(),
    ok.

init_per_testcase(registration_timeout, Config) ->
    inets:start(),
    meck:new(userdb_mysql_manager),
    meck:expect(userdb_mysql_manager, cast, fun (_) -> erlang:make_ref() end),
    Config;
init_per_testcase(_TestCase, Config) ->
    inets:start(),
    Config.

end_per_testcase(registration_timeout, _Config) ->
    meck:unload(userdb_mysql_manager),
    inets:stop(),
    ok;
end_per_testcase(_TestCase, _Config) ->
    inets:stop(),
    ok.

%%%===================================================================
%%% For other tests
%%%===================================================================
create_database_and_table() ->
    CreateDatabaseAndTableResult = os:cmd("mysql -uuserdb_test -puserdb_test < ../../../../test/priv/mysql-create-database-and-table.sql"),
    ?CONSOLE("CreateDatabaseAndTableResult: ~p~n", [CreateDatabaseAndTableResult]).

drop_database() ->
    DropDatabaseResult = os:cmd("mysql -uuserdb_test -puserdb_test < ../../../../test/priv/mysql-drop-database.sql"),
    ?CONSOLE("DropDatabaseResult: ~p~n", [DropDatabaseResult]).

%%%===================================================================
%%% Test cases
%%%===================================================================
registration(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    ?assertEqual(
        {ok, {200, "{\"description\":\"Success registration\"}"}},
        httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}])
    ),
    ?assertEqual(
        {ok,{400,"{\"description\":\"User already registered\"}"}},
        httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}])
    ).

registration_timeout(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    ?assertEqual(
        {ok, {408, "{\"description\":\"Registration timeout\"}"}},
        httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}])
    ).

registration_bad(_Config) ->
    ?assertEqual(
        {ok, {400, "{\"description\":\"Bad registration request\"}"}},
        httpc:request(post, {"http://localhost:8080/registration", [], "application/json", <<>>}, [], [{full_result, false}])
    ).
