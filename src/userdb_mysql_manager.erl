-module(userdb_mysql_manager).

-behaviour(gen_server).

-compile({parse_transform, lager_transform}).

-include("userdb_mysql_manager_api.hrl").
-include("userdb_msg.hrl").

-export([
    %% API
    start_link/1,
    %% mysql manager API
    cast/1,
    %% gen_server callbacks
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    mysql_pid :: undefined | pid()
}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Args) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%--------------------------------------------------------------------
%% @doc
%% Make cast
%%
%% @spec cast(Body :: session_manager_request_body()) -> Ref :: reference().
%% @end
%%--------------------------------------------------------------------
cast(Body) ->
    userdb_msg:cast(?MODULE, Body).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    lager:info("Init, Args:~p", [Args]),
    {ok, Pid} = mysql:start_link(Args),
    lager:info("MySQL pid:~p", [Pid]),
    erlang:link(Pid),
    {ok, #state{mysql_pid = Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Msg, _From, State) ->
    lager:info("Unknown handle_call, From: ~100p, Msg:~p", [_From, _Msg]),
    {reply, {error, unknown_msg}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(#userdb_msg{} = Msg, State) ->
    lager:debug("handle_cast, Msg:~p", [Msg]),
    Result = handle(Msg#userdb_msg.body, State),
    lager:debug("Result:~p", [Result]),
    WasSend = userdb_msg:reply(Msg, Result),
    lager:debug("WasSend:~p", [WasSend]),
    {noreply, State};
handle_cast(_Msg, State) ->
    lager:warning("Unknown handle_cast, Msg:~p", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:info("terminate, Reason: ~p", [_Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec handle(Msg :: userdb_mysql_manager_request(), State :: state()) -> Msg2 :: userdb_mysql_manager_response().
handle(#registration_request{} = Body, #state{} = State) ->
    lager:debug("handle, Body: ~p", [Body]),
    Columns = lists:join(<<", ">>, [<<"user">>, <<"password">>]),
    Values = lists:join(<<", ">>, [value(Value)|| Value <- [Body#registration_request.user, Body#registration_request.password]]),
    Query = [<<"INSERT INTO user (">>, Columns, <<") VALUES (">>, Values, <<")">>],
    %io:format(user, "-->Query:~p~n", [Query]),
    case mysql:query(State#state.mysql_pid, Query) of
        ok ->
            #registration_response{success = true, description = <<"Success registration">>};
        {error, {1062, _, _}} ->
            #registration_response{success = false, description = <<"User already registered">>};
        _ ->
            #registration_response{success = false, description = <<"Unsuccess registration">>}
    end;
handle(#authorization_request{} = Body, #state{} = State) ->
    lager:debug("handle, Body: ~p", [Body]),
    Query = [
        <<"SELECT EXISTS ( SELECT user FROM user WHERE `user`=">>,
        value(Body#authorization_request.user), <<" AND `password`=">>,
        value(Body#authorization_request.password), <<")">>
    ],
    %io:format(user, "-->Query:~p~n", [Query]),
    Result = mysql:query(State#state.mysql_pid, Query),
    %io:format(user, "-->Result:~p~n", [Result]),
    case Result of
        {ok, _ColumnNames, [[1]]} ->
            #authorization_response{success = true, description = <<"Success authorization">>};
        {ok, _, _} ->
            #authorization_response{success = false, description = <<"Bad user or password">>};
        _ ->
            #authorization_response{success = false, description = <<"Unsuccess authorization">>}
    end;
handle(#get_users_list_request{offset = Offset, limit = Limit} = _Body, #state{} = State) when
erlang:is_integer(Offset) andalso (Offset >= 0) andalso
erlang:is_integer(Limit) andalso (Limit > 0) andalso (Limit =< 50) ->
    lager:debug("handle, Body: ~p", [_Body]),
    %% Позже можно воспользоваться оптимизацией поиска https://habr.com/ru/post/217521/
    Query = [
        <<"SELECT `user` FROM `user` ORDER BY `user` LIMIT ">>,
        value(Offset), <<", ">>, value(Limit)
    ],
    %io:format(user, "-------------->Query:~p<---------------------", [Query]),
    Result = mysql:query(State#state.mysql_pid, Query),
    %io:format(user, "-------------->Result:~p<---------------------", [Result]),
    case Result of
        {ok, _, [List]} ->
            #get_users_list_response{success = true, list = List};
        _ ->
            #get_users_list_response{success = false, description = <<"Unsuccess get users list request">>}
    end.

-spec value(Value :: binary() | integer()) -> Value2 :: binary().
value(<<Value/binary>>) ->
    <<"'", (escape(Value))/binary, "'">>;
value(Value) when erlang:is_integer(Value) ->
    erlang:integer_to_binary(Value).

-spec escape(Value :: binary()) -> Value2 :: binary().
escape(<<Value/binary>>) ->
    binary:replace(
        Value,
        [
            <<0>>,
            <<"\"">>,
            <<"\n">>,
            <<"\r">>,
            <<26>>,
            <<"\t">>,
            <<"\b">>,
            <<"\\">>,
            <<"'">>
        ],
        <<"\\">>,
        [
            global,
            {insert_replaced, 1}
        ]
    ).
