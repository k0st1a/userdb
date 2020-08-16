-module(userdb_app).

-behaviour(application).

-compile({parse_transform, lager_transform}).

-export([
    start/2,
    prep_stop/1,
    stop/1
]).

start(_StartType, _StartArgs) ->
    lager:info("Start, _StartType: ~1000p, _StartArgs: ~1000p", [_StartType, _StartArgs]),
    Dispatch =
        cowboy_router:compile([
            {'_', [
                    {"/", userdb_cowboy_handler, []}
            ]}
        ]),
    {ok, _} = cowboy:start_clear(userdb_cowboy_handler, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    userdb_sup:start_link().

prep_stop(State) ->
    lager:info("Prep stop, State: ~1000p", [State]),
    ok = ranch:suspend_listener(userdb_cowboy_handler),
    ok = ranch:wait_for_connections(userdb_cowboy_handler, '==', 0),
    ok = ranch:stop_listener(userdb_cowboy_handler),
    State.

stop(_State) ->
    lager:info("Stop, _State: ~1000p", [_State]),
    ok.
