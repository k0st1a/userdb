-module(userdb_session_manager).

-behaviour(gen_server).

-compile({parse_transform, lager_transform}).

-include("userdb_session_manager_api.hrl").
-include("userdb_msg.hrl").
-include("userdb_session.hrl").

%% API
-export([
    %% API
    start_link/1,
    %% Session manager API
    cast/1,
    find_session/1,
    find/1,
    %% gen_server callbacks
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(R2P(Record, Value), lists:zip(record_info(fields, Record), erlang:tl(erlang:tuple_to_list(Value)))).
-define(DEFAULT_TTL, 60000).

-record(state, {
    session_ttl :: non_neg_integer()
}).
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
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

%%--------------------------------------------------------------------
%% @doc
%% Find session
%%
%% @spec find_session(Id :: binary() -> list().
%% @end
%%--------------------------------------------------------------------
find_session(Id) ->
    Timestamp = erlang:timestamp(),
    lager:debug("Find session, Id:~100p, Timestamp:~100p", [Id, Timestamp]),
    Filter =
        #session_filter{
            id = Id,
            user = '$1',
            expires = '$2',
            match_conditions = [{'>', '$2', {Timestamp}}],
            match_return = ['$1']
        },
    find(Filter).

%%--------------------------------------------------------------------
%% @doc
%% Find by filter
%%
%% @spec find(Filter :: session_filter()) -> list().
%% @end
%%--------------------------------------------------------------------
find(#session_filter{} = Filter) ->
    lager:debug("Find, proplists: ~p", [?R2P(session_filter, Filter)]),
    MCMatch =
        #session{
            id = Filter#session_filter.id,
            user = Filter#session_filter.user,
            expires = Filter#session_filter.expires,
            _ = '_'
        },
    MatchSpec = [{MCMatch, Filter#session_filter.match_conditions, Filter#session_filter.match_return}],
    Found = ets:select(?MODULE, MatchSpec),
    lager:debug("Found: ~p", [Found]),
    Found.

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
    ets:new(?MODULE, [
        ordered_set,
        protected,
        named_table,
        {read_concurrency, true},
        {keypos, #session.id}
    ]),
    SessionTTL2 =
        case lists:keyfind(session_ttl, 1, Args) of
            {_, SessionTTL} ->
                case check_session_ttl(SessionTTL) of
                    true ->
                        SessionTTL;
                    _ ->
                        ?DEFAULT_TTL
                end;
            _ ->
                ?DEFAULT_TTL
        end,
    State = #state{
        session_ttl = SessionTTL2
    },
    {ok, State}.

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
handle_info(_Info, State) ->
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
    lager:info("Terminate, Reason: ~p", [_Reason]),
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
-spec check_session_ttl(Value :: non_neg_integer()) -> boolean().
check_session_ttl(Value) ->
    erlang:is_integer(Value) andalso
    (Value > 0) andalso
    (Value =< 3600000).

-spec handle(Msg :: make_session_request(), State :: state()) -> Msg2 :: make_session_response().
handle(#make_session_request{user_name = Name} = _Body, #state{session_ttl = TTL}) ->
    lager:debug("handle, Body: ~p", [_Body]),
    Id = erlang:list_to_binary(erlang:ref_to_list(erlang:make_ref())),
    Timestamp = erlang:timestamp(),
    Expires = new_timestampt(Timestamp, TTL),
    lager:debug("Name:~100p, Id:~100p, Timestamp:~1000p, TTL:~1000p, Expires:~100p", [Name, Id, Timestamp, TTL, Expires]),
    ets:insert(
        ?MODULE,
        #session{
            id = Id,
            user = Name,
            expires = Expires
        }
    ),
    #make_session_response{session_id = Id}.

-spec new_timestampt(Timestamp :: erlang:timestamp(), MilliSeconds :: non_neg_integer()) -> Timestamp2 :: erlang:timestamp().
new_timestampt(Timestamp, MilliSeconds) ->
    Ticks = timestamp_to_ticks(Timestamp) + MilliSeconds * 1000,
    ticks_to_timestamp(Ticks).


-spec ticks_to_timestamp(Ticks :: non_neg_integer()) -> Timestamp :: erlang:timestamp().
ticks_to_timestamp(Ticks) ->
    {Ticks div 1000000000000, Ticks rem 1000000000000 div 1000000, Ticks rem 1000000}.

-spec timestamp_to_ticks(Timestamp :: erlang:timestamp()) -> Ticks :: non_neg_integer().
timestamp_to_ticks({MegaSeconds, Seconds, Microseconds}) ->
    MegaSeconds * 1000000000000 + Seconds * 1000000 + Microseconds.
