-module(userdb_cowboy_handler_registration).

-compile({parse_transform, lager_transform}).

-include("userdb_mysql_manager_api.hrl").
-include("userdb_timer.hrl").
-include("userdb_msg.hrl").

-export([
    %% cowboy handler callbacks
    init/2,
    info/3,
    terminate/3,
    %% External API
    read_body/1,
    decode/1,
    reply/3,
    start_timer/1,
    cancel_timer/1
]).

-record(state, {
    request_ref :: reference(),
    timer_ref :: reference()
}).

init(Req, Opts) ->
    lager:info("Init, Req:~p~nOpts:~n~p", [Req, Opts]),
    %% Необходимый нам данные находятся в теле запроса в формате JSON
    {ok, Body, Req2} = read_body(Req),
    lager:debug("Body:~n~p", [Body]),
    Decoded = decode(Body),
    case Decoded of
        #{<<"user">> := <<User/binary>>, <<"password">> := <<Password/binary>>} when
        ((erlang:size(User) > 0) andalso (erlang:size(User) < 26)) andalso
        ((erlang:size(Password) > 0) andalso (erlang:size(Password) < 26)) ->
            RequestRef = userdb_mysql_manager:cast(#registration_request{user = User, password = Password}),
            TimerRef = start_timer(?TIMER_REGISTRATION),
            lager:debug("Cast registration_request, User:~100p, RequestRef:~100p, TimerRef:~100p", [User, RequestRef, TimerRef]),
            {cowboy_loop, Req2, #state{request_ref = RequestRef, timer_ref = TimerRef}};
        _ ->
            lager:debug("Bad json, Decoded:~n~p", [Decoded]),
            {stop, reply(Req2, 400, <<"{\"description\":\"Bad json\"}">>), #state{}}
    end.

info(#userdb_msg{body = #registration_response{} = Body, options = #{ref := Ref}} = Msg, Req, #state{request_ref = Ref} = State) ->
    lager:debug("Info, Msg:~p", [Msg]),
    cancel_timer(State#state.timer_ref),
    JSON = <<"{\"description\":\"", (Body#registration_response.description)/binary, "\"}">>,
    case Body#registration_response.success of
        true ->
            {stop, reply(Req, 200, JSON), State};
        _ ->
            {stop, reply(Req, 400, JSON), State}
    end;
info({timeout, TimerRef, #timer{id = ?TIMER_REGISTRATION = _Id}}, Req, #state{timer_ref = TimerRef} = State) ->
    lager:debug("Info, Fired timer, Id:~100p, TimerRef:~100p", [_Id, TimerRef]),
    {stop, cowboy_req:reply(408, Req), State};

info(_Msg, Req, State) ->
    lager:debug("Info, skip Msg:~p", [_Msg]),
    {ok, Req, State}.

terminate(_Reason, _Req, _Opts) ->
    lager:info("Terminate, Reason:~100p", [_Reason]),
    ok.

%% External API
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

-spec start_timer(Id :: atom()) -> Ref :: reference().
start_timer(Id) ->
    Ref = erlang:start_timer(?CAST_TIMEOUT, self(), #timer{id = Id}),
    lager:debug("Start timer, Id:~100p, Ref:~100p", [Id, Ref]),
    Ref.

-spec cancel_timer(Ref :: reference()) -> term().
cancel_timer(Ref) ->
    lager:debug("Cancel timer, Ref:~100p", [Ref]),
    erlang:cancel_timer(Ref, [{info, false}]).
