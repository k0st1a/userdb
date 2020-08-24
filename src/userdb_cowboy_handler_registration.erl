-module(userdb_cowboy_handler_registration).

-compile({parse_transform, lager_transform}).

-include("userdb_mysql_manager_api.hrl").
-include("userdb_timer.hrl").
-include("userdb_msg.hrl").

%% cowboy handler callbacks
-export([
    init/2,
    info/3,
    terminate/3
]).

%% External API
-export([
    get_user_and_password/1
]).

-record(state, {
    request_ref :: reference(),
    timer_ref :: reference()
}).

init(Req, _) ->
    lager:info("Init, Req:~p", [Req]),
    {ok, Body, Req2} = userdb_utils:read_body(Req),
    lager:debug("Body:~n~p", [Body]),
    case get_user_and_password(Body) of
        {ok, User, Password} ->
            RequestRef = userdb_mysql_manager:cast(#registration_request{user = User, password = Password}),
            TimerRef = userdb_timer:start(?TIMER_REGISTRATION),
            lager:debug("Cast registration_request, User:~100p, RequestRef:~100p, TimerRef:~100p", [User, RequestRef, TimerRef]),
            {cowboy_loop, Req2, #state{request_ref = RequestRef, timer_ref = TimerRef}};
        _ ->
            JSON = jsx:encode([{<<"description">>, <<"Bad registration request">>}]),
            {ok, userdb_utils:reply(Req2, 400, JSON), #state{}}
    end.

info(#userdb_msg{body = #registration_response{} = Body, options = #{ref := Ref}} = Msg, Req, #state{request_ref = Ref} = State) ->
    lager:debug("Info, Msg:~p", [Msg]),
    userdb_timer:cancel(State#state.timer_ref),
    JSON = jsx:encode([{<<"description">>, Body#registration_response.description}]),
    case Body#registration_response.success of
        true ->
            {stop, userdb_utils:reply(Req, 200, JSON), State};
        _ ->
            {stop, userdb_utils:reply(Req, 400, JSON), State}
    end;
info({timeout, TimerRef, #timer{id = ?TIMER_REGISTRATION = _Id}}, Req, #state{timer_ref = TimerRef} = State) ->
    lager:debug("Info, Fired timer, Id:~100p, TimerRef:~100p", [_Id, TimerRef]),
    JSON = jsx:encode([{<<"description">>, <<"Registration timeout">>}]),
    {stop, userdb_utils:reply(Req, 500, JSON), State};

info(_Msg, Req, State) ->
    lager:debug("Info, skip Msg:~p", [_Msg]),
    {ok, Req, State}.

terminate(_Reason, _Req, _State) ->
    lager:info("Terminate, Reason:~100p", [_Reason]),
    ok.

%% External API
-spec get_user_and_password(Body :: binary()) -> {ok, User :: binary(), Password :: binary} | error.
get_user_and_password(Body) ->
    Decoded = userdb_utils:decode(Body),
    case Decoded of
        #{<<"user">> := User, <<"password">> := Password} ->
            case userdb_utils:check_user(User) andalso userdb_utils:check_password(Password) of
                true ->
                    {ok, User, Password};
                _ ->
                    lager:debug("Bad user or password", []),
                    error
            end;
        _ ->
            lager:debug("Bad json, Decoded:~n~p", [Decoded]),
            error
    end.
