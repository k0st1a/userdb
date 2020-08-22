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
    case userdb_utils:find_session(Req) of
        {ok, User} ->
            lager:debug("User:~100p", [User]),
            {ok, Body, Req2} = userdb_utils:read_body(Req),
            lager:debug("Body:~n~p", [Body]),
            case read_parameters(Body) of
                #{offset := Offset, limit := Limit} ->
                    RequestRef = userdb_mysql_manager:cast(#get_users_list_request{offset = Offset, limit = Limit}),
                    TimerRef = userdb_timer:start(?TIMER_GET_USERS_LIST),
                    lager:debug("Cast get_user_list_request, User:~100p, RequestRef:~100p, TimerRef:~100p", [User, RequestRef, TimerRef]),
                    {cowboy_loop, Req2, #state{request_ref = RequestRef, timer_ref = TimerRef}};
                _ ->
                    {ok, userdb_utils:reply(Req, 400, <<"{\"description\":\"Bad offset or limit\"}">>), #state{}}
            end;
        {error, Description} ->
            lager:debug("Description:~p", [Description]),
            {ok, userdb_utils:reply(Req, 400, <<"{\"description\":\"", Description/binary, "\"}">>), #state{}}
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
    {stop, userdb_utils:reply(Req, 500, <<"{\"description\":\"Get users list timeout\"}">>), State};

info(_Msg, Req, State) ->
    lager:debug("Info, skip Msg:~p", [_Msg]),
    {ok, Req, State}.

terminate(_Reason, _Req, _Opts) ->
    lager:info("Terminate, Reason:~100p", [_Reason]),
    ok.

%% Internal API
-spec read_parameters(Body :: binary()) -> Parameters :: map() | error.
read_parameters(Body) ->
    Decoded = userdb_utils:decode(Body),
    case Decoded of
        #{} ->
            Offset = maps:get(<<"offset">>, Decoded, 0),
            lager:debug("Offset:~p", [Offset]),
            Limit = maps:get(<<"limit">>, Decoded, 50),
            lager:debug("Limit:~p", [Limit]),
            case userdb_utils:check_offset(Offset) andalso userdb_utils:check_limit(Limit) of
                true ->
                    #{offset => Offset, limit => Limit};
                _ ->
                    error
            end;
        _ ->
            lager:debug("Bad json, Decoded:~n~p", [Decoded]),
            error
    end.
