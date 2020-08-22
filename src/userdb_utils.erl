-module(userdb_utils).

-compile({parse_transform, lager_transform}).

-export([
    decode/1,
    read_body/1,
    reply/3,
    check_user/1,
    check_password/1,
    check_offset/1,
    check_limit/1,
    find_session/1
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

-spec check_user(Value :: binary()) -> boolean().
check_user(Value) ->
    erlang:is_binary(Value) andalso
    (erlang:size(Value) > 0) andalso
    (erlang:size(Value) < 26).

-spec check_password(Value :: binary()) -> boolean().
check_password(Value) ->
    erlang:is_binary(Value) andalso
    (erlang:size(Value) > 0) andalso
    (erlang:size(Value) < 26).

-spec check_offset(Value :: non_neg_integer()) -> boolean().
check_offset(Value) ->
    erlang:is_integer(Value) andalso
    (Value >= 0).

-spec check_limit(Value :: pos_integer()) -> boolean().
check_limit(Value) ->
    erlang:is_integer(Value) andalso
    (Value > 0) andalso
    (Value =< 50).

-spec find_session(Req :: cowboy_req:req()) -> {ok, User :: binary()} | {error, Description :: binary()}.
find_session(Req) ->
    lager:debug("Find session", []),
    Cookies = cowboy_req:parse_cookies(Req),
    lager:debug("Cookies:~p", [Cookies]),
    case lists:keyfind(<<"session_id">>, 1, Cookies) of
        {_, SessionId} ->
            lager:debug("SessionId:~100p", [SessionId]),
            case userdb_session_manager:find_session(SessionId) of
                [User| _] ->
                    {ok, User};
                _ ->
                    lager:debug("Unauthorized", []),
                    {error, <<"Unauthorized">>}
            end;
        _ ->
            lager:debug("Not found session_id", []),
            {error, <<"Not found session_id in cookie">>}
    end.
