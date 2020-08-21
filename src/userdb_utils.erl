-module(userdb_utils).

-export([
    decode/1,
    read_body/1,
    reply/3
]).

-spec decode(Body :: term()) -> Decoded :: map() | error.
decode(Body) ->
    try
        jsx:decode(Body, [return_maps])
    catch _:_ ->
        error
    end.

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

-spec reply(Req :: cowboy_req:req(), Status :: pos_integer(), Body :: binary()) -> Req2 :: cowboy_req:req().
reply(Req, Status, Body) ->
    cowboy_req:reply(
        Status,
        #{<<"content-type">> => <<"application/json; charset=utf-8">>},
        Body,
        Req
    ).
