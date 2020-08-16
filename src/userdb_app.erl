%%%-------------------------------------------------------------------
%% @doc userdb public API
%% @end
%%%-------------------------------------------------------------------

-module(userdb_app).

-behaviour(application).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    userdb_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
