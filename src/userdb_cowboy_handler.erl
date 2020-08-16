-module(userdb_cowboy_handler).

-compile({parse_transform, lager_transform}).

-export([
    %% cowboy handler callback
    init/2
]).

init(Req, Opts) ->
    lager:debug("Init~n-->Req:~n~p~n--->Opts:~n ~p", [Req, Opts]),
    Req2 = cowboy_req:reply(200, #{<<"content-type">> => <<"text/plain">>}, <<"Hello world!">>, Req),
    lager:debug("-->Req2:~n~p", [Req2]),
    {ok, Req2, Opts}.
