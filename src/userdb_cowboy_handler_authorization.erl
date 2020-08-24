-module(userdb_cowboy_handler_authorization).

-compile({parse_transform, lager_transform}).

-include("userdb_mysql_manager_api.hrl").
-include("userdb_session_manager_api.hrl").
-include("userdb_timer.hrl").
-include("userdb_msg.hrl").

-export([
    %% cowboy handler callbacks
    init/2,
    info/3,
    terminate/3
]).

-record(state, {
    user :: binary(),
    request_ref :: reference(),
    timer_ref :: reference()
}).

init(Req, _) ->
    lager:info("Init, Req:~p~n", [Req]),
    {ok, Body, Req2} = userdb_utils:read_body(Req),
    lager:debug("Body:~n~p", [Body]),
    case userdb_cowboy_handler_registration:get_user_and_password(Body) of
        {ok, User, Password} ->
            RequestRef = userdb_mysql_manager:cast(#authorization_request{user = User, password = Password}),
            TimerRef = userdb_timer:start(?TIMER_AUTHORIZATION),
            lager:debug("Cast authorization_request, User:~100p, RequestRef:~100p, TimerRef:~100p", [User, RequestRef, TimerRef]),
            {cowboy_loop, Req2, #state{request_ref = RequestRef, timer_ref = TimerRef, user = User}};
        _ ->
            JSON = jsx:encode([{<<"description">>, <<"Bad authorization request">>}]),
            {ok, userdb_utils:reply(Req2, 400, JSON), #state{}}
    end.

info(#userdb_msg{body = #authorization_response{} = Body, options = #{ref := Ref}} = Msg, Req, #state{request_ref = Ref, user = User} = State) ->
    lager:debug("Info, Msg:~p", [Msg]),
    userdb_timer:cancel(State#state.timer_ref),
    case Body#authorization_response.success of
        true ->
            RequestRef = userdb_session_manager:cast(#make_session_request{user_name = User}),
            TimerRef = userdb_timer:start(?TIMER_MAKE_SESSION),
            lager:debug("Cast make_session_request, User:~100p, RequestRef:~100p, TimerRef:~100p", [User, RequestRef, TimerRef]),
            {ok, Req, State#state{request_ref = RequestRef, timer_ref = TimerRef}};
        _ ->
            JSON = jsx:encode([{<<"description">>, Body#authorization_response.description}]),
            {stop, userdb_utils:reply(Req, 400, JSON), State}
    end;
info({timeout, TimerRef, #timer{id = ?TIMER_AUTHORIZATION = _Id}}, Req, #state{timer_ref = TimerRef} = State) ->
    lager:debug("Info, Fired timer, Id:~100p, TimerRef:~100p", [_Id, TimerRef]),
    JSON = jsx:encode([{<<"description">>, <<"Authorization timeout">>}]),
    {stop, userdb_utils:reply(Req, 500, JSON), State};

info(#userdb_msg{body = #make_session_response{} = Body, options = #{ref := Ref}} = Msg, Req, #state{request_ref = Ref} = State) ->
    lager:debug("Info, Msg:~p", [Msg]),
    Req2 = cowboy_req:set_resp_cookie(<<"session_id">>, Body#make_session_response.session_id, Req),
    userdb_timer:cancel(State#state.timer_ref),
    JSON = jsx:encode([{<<"description">>, <<"Success authorization">>}]),
    {stop, userdb_utils:reply(Req2, 200, JSON), State};
info({timeout, TimerRef, #timer{id = ?TIMER_MAKE_SESSION = _Id}}, Req, #state{timer_ref = TimerRef} = State) ->
    lager:debug("Info, Fired timer, Id:~100p, TimerRef:~100p", [_Id, TimerRef]),
    JSON = jsx:encode([{<<"description">>, <<"Make session timeout">>}]),
    {stop, userdb_utils:reply(Req, 500, JSON), State};

info(_Msg, Req, State) ->
    lager:debug("Info, skip Msg:~p", [_Msg]),
    {ok, Req, State}.

terminate(_Reason, _Req, _Opts) ->
    lager:info("Terminate, Reason:~100p", [_Reason]),
    ok.
