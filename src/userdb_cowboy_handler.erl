-module(userdb_cowboy_handler).

%% Делаем без сессий

-compile({parse_transform, lager_transform}).

-export([
    %% cowboy handler callbacks
    init/2,
   %info/3,
    terminate/3
]).

init(Req, Opts) ->
    lager:info("Init, Req:~p~nOpts:~n~p", [Req, Opts]),
    %% Необходимый нам данные находятся в теле запроса в формате JSON
    {ok, Body, Req2} = read_body(Req),
    lager:debug("Body:~n~p", [Body]),
    Decoded = decode(Body),
    lager:debug("Decoded:~n~p", [Decoded]),
    case Decoded of
        #{<<"action">> := <<"authorization">>} ->
            lager:debug("Action: authorization", []),
            case Decoded of
                %#{<<"action">> => <<"authorization">>,<<"password">> => <<"user password">>,<<"user">> => <<"it is user">>}
                #{<<"user">> := _, <<"password">> := _} ->
                    lager:debug("Well-formed request", []),
                    {ok, reply(Req2, 200, <<"{\"description\":\"Well-formed request\"}">>), Opts};
                _ ->
                    lager:debug("Not found user and/or password", []),
                    {ok, reply(Req2, 400, <<"{\"description\":\"Not found user and/or password\"}">>), Opts}
            end;
        #{<<"action">> := _Action} ->
            lager:debug("Bad action: ~p", [_Action]),
            {ok, reply(Req2, 400, <<"{\"description\":\"Bad action\"}">>), Opts};
        #{} ->
            lager:debug("Not found action: ~p", []),
            {ok, reply(Req2, 400, <<"{\"description\":\"Not found action\"}">>), Opts};
        _ ->
            lager:debug("Bad json", []),
            {ok, reply(Req2, 400, <<"{\"description\":\"Bad json\"}">>), Opts}
    end.

%   info({message, Msg}, Req, State) ->
%       lager:info("Msg:~p~n", [Msg]),
%       lager:info("Req:~p~n", [Req]),
%       lager:info("State:~p~n", [State]),
%       cowboy_req:stream_events(#{
%           id => id(),
%           data => Msg
%       }, nofin, Req),
%       %{stop, Req, State}.
%       erlang:send_after(1000, self(), {message, "Tick"}),
%       {ok, Req, State}.

terminate(Reason, Req, Opts) ->
    lager:info("Terminate, Reason: ~1000p, Req:~n~p~nOpts:~n~p", [Reason, Req, Opts]),
    ok.

-spec read_body(Req :: cowboy_req:req()) -> {ok, Data :: binary(), Req2 :: cowboy_req:req()}.
read_body(Req) ->
    read_body(Req, <<>>).

-spec read_body(Req :: cowboy_req:req(), Acc :: binary()) -> {ok, Data :: binary(), Req2 :: cowboy_req:req()}.
read_body(Req, Acc) ->
    case cowboy_req:read_body(Req) of
        {ok, Data, Req2} ->
            {ok, <<Acc/binary, Data/binary >>, Req2};
        {more, Data, Req2} ->
            read_body(Req2, <<Acc/binary, Data/binary >>)
    end.

-spec decode(Body :: term()) -> Decoded :: map() | error.
decode(Body) ->
    try
        jsx:decode(Body, [return_maps])
    catch _:_ ->
        error
    end.

-spec reply(Req :: cowboy_req:req(), Status :: pos_integer(), Body :: binary()) -> Req2 :: cowboy_req:req().
reply(Req, Status, Body) ->
    cowboy_req:reply(
        Status,
        #{<<"content-type">> => <<"application/json; charset=utf-8">>},
        Body,
        Req
    ).
