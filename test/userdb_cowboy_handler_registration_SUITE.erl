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
    bad_registration_request/1,
    success_registration/1,
    user_already_registered/1,
    unsuccess_registration/1,
    registration_timeout/1
]).

%% For other tests
-export([
    create_database_and_table/0,
    drop_database/0
]).

-include_lib("eunit/include/eunit.hrl").

-define(CONSOLE(Format, Args), io:format(user, "[~100p:~100p] " ++ Format, [?MODULE, ?LINE] ++ Args)).

%%%===================================================================
%%% Common Test callbacks
%%%===================================================================

all() -> [
    bad_registration_request,
    success_registration,
    user_already_registered,
    unsuccess_registration,
    registration_timeout
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

init_per_testcase(user_already_registered, Config) ->
    inets:start(),
    meck:new(mysql),
    meck:expect(mysql, query, fun (_, _) -> {error, {1062, <<>>, <<>>}} end),
    Config;
init_per_testcase(unsuccess_registration, Config) ->
    inets:start(),
    meck:new(mysql),
    meck:expect(mysql, query, fun (_, _) -> {error, unsuccess_request} end),
    Config;
init_per_testcase(registration_timeout, Config) ->
    inets:start(),
    meck:new(userdb_mysql_manager),
    meck:expect(userdb_mysql_manager, cast, fun (_) -> erlang:make_ref() end),
    Config;
init_per_testcase(_TestCase, Config) ->
    inets:start(),
    Config.

end_per_testcase(user_already_registered, _Config) ->
    meck:unload(mysql),
    inets:stop(),
    ok;
end_per_testcase(unsuccess_registration, _Config) ->
    meck:unload(mysql),
    inets:stop(),
    ok;
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
bad_registration_request(_Config) ->
    ?assertEqual(
        {ok, {400, "{\"description\":\"Bad registration request\"}"}},
        httpc:request(post, {"http://localhost:8080/registration", [], "application/json", <<>>}, [], [{full_result, false}])
    ).

success_registration(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    ?assertEqual(
        {ok, {200, "{\"description\":\"Success registration\"}"}},
        httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}])
    ).

user_already_registered(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    ?assertEqual(
        {ok,{400,"{\"description\":\"User already registered\"}"}},
        httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}])
    ).

unsuccess_registration(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    ?assertEqual(
        {ok,{400,"{\"description\":\"Unsuccess registration\"}"}},
        httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}])
    ).

registration_timeout(_Config) ->
    Data = <<"{\"user\":\"it is user\",\"password\":\"user password\"}">>,
    ?assertEqual(
        {ok, {500, "{\"description\":\"Registration timeout\"}"}},
        httpc:request(post, {"http://localhost:8080/registration", [], "application/json", Data}, [], [{full_result, false}])
    ).
