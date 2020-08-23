-module(userdb_timer).

-compile({parse_transform, lager_transform}).

-include("userdb_timer.hrl").

-export([
    start/1,
    start/2,
    start/3,
    cancel/1
]).

-spec start(Id :: atom()) -> Ref :: reference().
start(Id) ->
    start(Id, ?CAST_TIMEOUT, #{}).

-spec start(Id :: atom(), Timeout :: non_neg_integer()) -> Ref :: reference().
start(Id, Timeout) ->
    start(Id, Timeout, #{}).

-spec start(Id :: atom(), Timeout :: non_neg_integer(), Options :: map()) -> Ref :: reference().
start(Id, Timeout, Options) when erlang:is_integer(Timeout) andalso (Timeout >= 0) ->
    Ref = erlang:start_timer(Timeout, self(), #timer{id = Id, options = Options}),
    lager:debug("Start timer, Id:~100p, Ref:~100p", [Id, Ref]),
    Ref.

-spec cancel(Ref :: reference()) -> term().
cancel(Ref) ->
    lager:debug("Cancel timer, Ref:~100p", [Ref]),
    erlang:cancel_timer(Ref, [{info, false}]).
