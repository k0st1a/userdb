-module(userdb_cowboy_handler_get_users_list).

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
    Cookies = cowboy_req:parse_cookies(Req),
    lager:debug("Cookies:~p", [Cookies]),
    case lists:keyfind(<<"session_id">>, 1, Cookies) of
        {_, SessionId} ->
            lager:debug("SessionId:~100p", [SessionId]),
            case userdb_session_manager:find_session(SessionId) of
                [User| _] ->
                    lager:debug("User:~100p", [User]),
                    {ok, Body, Req2} = userdb_utils:read_body(Req),
                    lager:debug("Body:~n~p", [Body]),
                    case get_parameters(Body) of
                        #{offset := Offset, limit := Limit} ->
                            RequestRef = userdb_mysql_manager:cast(#get_users_list_request{offset = Offset, limit = Limit}),
                            TimerRef = userdb_timer:start(?TIMER_GET_USERS_LIST),
                            lager:debug("Cast get_user_list_request, User:~100p, RequestRef:~100p, TimerRef:~100p", [User, RequestRef, TimerRef]),
                            {cowboy_loop, Req2, #state{request_ref = RequestRef, timer_ref = TimerRef}};
                        _ ->
                            {ok, userdb_utils:reply(Req, 400, <<"{\"description\":\"Bad offset or limit\"}">>), #state{}}
                    end;
                _ ->
                    lager:debug("Unauthorized", []),
                    {ok, userdb_utils:reply(Req, 400, <<"{\"description\":\"Unauthorized\"}">>), #state{}}
            end;
        _ ->
            lager:debug("Not found session_id", []),
            {ok, userdb_utils:reply(Req, 400, <<"{\"description\":\"Not found session_id in cookie\"}">>), #state{}}
    end.

info(#userdb_msg{body = #get_users_list_response{} = Body, options = #{ref := Ref}} = Msg, Req, #state{request_ref = Ref} = State) ->
    lager:debug("Info, Msg:~p", [Msg]),
    userdb_timer:cancel(State#state.timer_ref),
    case Body#get_users_list_response.success of
        true ->
            JSON = <<"{\"list\":\"", (unicode:characters_to_binary(lists:join(<<",">>, Body#get_users_list_response.list)))/binary, "\"}">>,
            {stop, userdb_utils:reply(Req, 200, JSON), State};
        _ ->
            JSON = <<"{\"description\":\"", (Body#get_users_list_response.description)/binary, "\"}">>,
            {stop, userdb_utils:reply(Req, 400, JSON), State}
    end;
info({timeout, TimerRef, #timer{id = ?TIMER_GET_USERS_LIST = _Id}}, Req, #state{timer_ref = TimerRef} = State) ->
    lager:debug("Info, Fired timer, Id:~100p, TimerRef:~100p", [_Id, TimerRef]),
    {stop, userdb_utils:reply(Req, 400, <<"{\"description\":\"Get users list timeout\"}">>), State};

info(_Msg, Req, State) ->
    lager:debug("Info, skip Msg:~p", [_Msg]),
    {ok, Req, State}.

terminate(_Reason, _Req, _Opts) ->
    lager:info("Terminate, Reason:~100p", [_Reason]),
    ok.

%% Internal API
-spec get_parameters(Body :: binary()) -> {ok, User :: binary(), Password :: binary} | error.
get_parameters(Body) ->
    Decoded = userdb_utils:decode(Body),
    case userdb_utils:decode(Body) of
        #{} = Decoded ->
            Offset = maps:get(<<"offset">>, Decoded, 0),
            lager:debug("Offset:~p", [Offset]),
            Limit = maps:get(<<"limit">>, Decoded, 50),
            lager:debug("Limit:~p", [Limit]),
            if
                erlang:is_integer(Offset) andalso
                (Offset >= 0) andalso
                erlang:is_integer(Limit) andalso
                (Limit > 0) andalso (Limit =< 50) ->
                    #{offset => Offset, limit => Limit};
                true ->
                    error
            end;
        _ ->
            lager:debug("Bad json, Decoded:~n~p", [Decoded]),
            error
    end.
