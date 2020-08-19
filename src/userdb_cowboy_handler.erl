-module(userdb_cowboy_handler).

-compile({parse_transform, lager_transform}).

-include("userdb_session_manager_api.hrl").

-export([
    %% cowboy handler callbacks
    init/2,
    info/3,
    terminate/3
]).

-record(state, {
    request_ref :: reference(),
    timer_ref :: reference()
}).

-define(TIMER_MAKE_SESSION, timer_make_session).
-type timer_id() :: ?TIMER_MAKE_SESSION.

-record(timer, {
    id :: timer_id()
}).
-define(REQUEST_TIMEOUT_MAKE_SESSION, 1000).

init(Req, Opts) ->
    lager:info("Init, Req:~p~nOpts:~n~p", [Req, Opts]),
    %% Необходимый нам данные находятся в теле запроса в формате JSON
    {ok, Body, Req2} = read_body(Req),
    lager:debug("Body:~n~p", [Body]),
    Decoded = decode(Body),
    lager:debug("Decoded:~n~p", [Decoded]),
    case Decoded of
        #{<<"action">> := <<"registration">>, <<"user">> := _, <<"password">> := _} ->
            lager:debug("Action: registration", []),
            {ok, reply(Req2, 200, <<"{\"description\":\"Well-formed registration request\"}">>), Opts};
        #{<<"action">> := <<"authorization">>, <<"user">> := User} ->
            lager:debug("Action: authorization", []),
            RequestRef = userdb_session_manager:cast(#make_session_request{user_name = User}),
            TimerRef = erlang:start_timer(?REQUEST_TIMEOUT_MAKE_SESSION, self(), #timer{id = ?TIMER_MAKE_SESSION}),
            {cowboy_loop, Req2, #state{request_ref = RequestRef, timer_ref = TimerRef}};
        #{<<"action">> := <<"change_user_password">>, <<"user">> := _, <<"password">> := _, <<"new_password">> := _} ->
            lager:debug("Action: change_user_password", []),
            {ok, reply(Req2, 200, <<"{\"description\":\"Well-formed change_user_password request\"}">>), Opts};
        #{<<"action">> := <<"get_users_list">>} ->
            lager:debug("Action: get_users_list", []),
            {ok, reply(Req2, 200, <<"{\"description\":\"Well-formed get_users_list request\"}">>), Opts};
        _ ->
            lager:debug("Bad json", []),
            {ok, reply(Req2, 400, <<"{\"description\":\"Bad json\"}">>), Opts}
    end.

info(#sm_msg{body = #make_session_response{}, options = #{ref := Ref}} = Msg, Req, #state{request_ref = Ref} = State) ->
    lager:debug("Info, Msg:~p", [Msg]),
    Req3 = cowboy_req:set_resp_cookie(<<"session_id">>, Msg#sm_msg.body#make_session_response.session_id, Req),
    erlang:cancel_timer(State#state.timer_ref, [{info, false}]),
    {stop, reply(Req3, 200, <<"{\"description\":\"Success authorization\"}">>), State};

info({timeout, TimerRef, #timer{id = ?TIMER_MAKE_SESSION}} = Msg, Req, #state{timer_ref = TimerRef} = State) ->
    lager:debug("Info, Msg:~p", [Msg]),
    {stop, reply(Req, 408, <<"{\"description\":\"Authorization timeout\"}">>), State};

info(_Msg, Req, State) ->
    lager:debug("Info, skip Msg:~p", [_Msg]),
    {ok, Req, State}.

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
