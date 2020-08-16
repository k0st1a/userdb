-module(userdb).

-compile({parse_transform, lager_transform}).

-export([
    start/0,
    stop/0,
    restart/0,
    lager/0,
    async/0
]).

-spec start() -> {ok, Started :: [atom()]} | {error, Reason :: term()}.
start() ->
    lager:info("Start", []),
    Result = application:ensure_all_started(userdb),
    lager:info("Result: ~p", [Result]),
    Result.

-spec stop() -> ok | {error, Reason :: term()}.
stop() ->
    lager:info("Stop", []),
    Result = application:stop(userdb),
    lager:info("Result: ~p", [Result]),
    Result.

-spec restart() -> {ok, Started :: [atom()]} | {error, Reason :: term()}.
restart() ->
    lager:info("Restart", []),
    stop(),
    start().

-spec lager() -> {ok, Started :: [atom()]} | {error, Reason :: term()}.
lager() ->
    Result = application:ensure_all_started(lager),
    lager:info("Lager, Result: ~p", [Result]),
    ok.

-spec async() -> {ok, Started :: [atom()]} | {error, Reason :: term()}.
async() ->
    Result = application:ensure_all_started(async),
    lager:info("Async, Result: ~p", [Result]),
    ok.
