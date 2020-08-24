-module(userdb_cowboy_handler_change_user_password).

-compile({parse_transform, lager_transform}).

-include("userdb_mysql_manager_api.hrl").
-include("userdb_timer.hrl").
-include("userdb_msg.hrl").

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

init(Req, _) ->
    lager:info("Init, Req:~p~n", [Req]),
    case userdb_utils:find_session(Req) of
        {ok, User} ->
            lager:debug("User:~100p", [User]),
            {ok, Body, Req2} = userdb_utils:read_body(Req),
            lager:debug("Body:~n~p", [Body]),
            case read_parameters(Body) of
                {ok, Password, NewPassword} ->
                    RequestRef = userdb_mysql_manager:cast(#change_user_password_request{user = User, password = Password, new_password = NewPassword}),
                    TimerRef = userdb_timer:start(?TIMER_CHANGE_USER_PASSWORD),
                    lager:debug("Cast change_user_password_request, User:~100p, RequestRef:~100p, TimerRef:~100p", [User, RequestRef, TimerRef]),
                    {cowboy_loop, Req2, #state{request_ref = RequestRef, timer_ref = TimerRef}};
                {error, Description} ->
                    lager:debug("Description:~p", [Description]),
                    JSON = jsx:encode([{<<"description">>, Description}]),
                    {ok, userdb_utils:reply(Req, 400, JSON), #state{}}
            end;
        {error, Description} ->
            lager:debug("Description:~p", [Description]),
            JSON = jsx:encode([{<<"description">>, Description}]),
            {ok, userdb_utils:reply(Req, 400, JSON), #state{}}
    end.

info(#userdb_msg{body = #change_user_password_response{} = Body, options = #{ref := Ref}} = Msg, Req, #state{request_ref = Ref} = State) ->
    lager:debug("Info, Msg:~p", [Msg]),
    userdb_timer:cancel(State#state.timer_ref),
    JSON = jsx:encode([{<<"description">>, Body#change_user_password_response.description}]),
    case Body#change_user_password_response.success of
        true ->
            {stop, userdb_utils:reply(Req, 200, JSON), State};
        _ ->
            {stop, userdb_utils:reply(Req, 400, JSON), State}
    end;
info({timeout, TimerRef, #timer{id = ?TIMER_CHANGE_USER_PASSWORD = _Id}}, Req, #state{timer_ref = TimerRef} = State) ->
    lager:debug("Info, Fired timer, Id:~100p, TimerRef:~100p", [_Id, TimerRef]),
    JSON = jsx:encode([{<<"description">>, <<"Change user password timeout">>}]),
    {stop, userdb_utils:reply(Req, 500, JSON), State};

info(_Msg, Req, State) ->
    lager:debug("Info, skip Msg:~p", [_Msg]),
    {ok, Req, State}.

terminate(_Reason, _Req, _Opts) ->
    lager:info("Terminate, Reason:~100p", [_Reason]),
    ok.

%% Internal API
-spec read_parameters(Body :: binary()) -> {ok, Pass :: binary(), Password :: binary} | error.
read_parameters(Body) ->
    case userdb_utils:decode(Body) of
        #{<<"password">> := Password, <<"new_password">> := NewPassword} ->
            case userdb_utils:check_password(Password) andalso userdb_utils:check_password(NewPassword) of
                true ->
                    {ok, Password, NewPassword};
                _ ->
                    {error, <<"Bad password or new password">>}
            end;
        _ ->
            {error, <<"Bad json">>}
    end.
