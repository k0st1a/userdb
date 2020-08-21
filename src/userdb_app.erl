-module(userdb_app).

-behaviour(application).

-compile({parse_transform, lager_transform}).

-export([
    start/2,
    prep_stop/1,
    stop/1
]).

start(_StartType, _StartArgs) ->
    lager:info("Start", []),
    Dispatch =
        cowboy_router:compile([
            {'_', [
                    {"/registration", userdb_cowboy_handler_registration, undefined},
                    {"/authorization", userdb_cowboy_handler_authorization, undefined},
                    {"/get_users_list", userdb_cowboy_handler_get_users_list, undefined}
            ]}
        ]),
    TransportOptions = [
        {port, 8080}
    ],
    ProtocolOptions = #{env => #{dispatch => Dispatch}},
    case cowboy:start_clear(userdb_http_listener, TransportOptions, ProtocolOptions) of
        {ok, _} ->
            userdb_sup:start_link();
        {error, _} = Error ->
            Error
    end.

prep_stop(State) ->
    lager:info("Prep stop", []),
    ok = ranch:suspend_listener(userdb_http_listener),
    ok = ranch:wait_for_connections(userdb_http_listener, '==', 0),
    ok = ranch:stop_listener(userdb_http_listener),
    State.

stop(_State) ->
    lager:info("Stop", []),
    ok.
