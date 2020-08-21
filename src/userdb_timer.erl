-module(userdb_timer).

-compile({parse_transform, lager_transform}).

-include("userdb_timer.hrl").

-export([
    start/1,
    cancel/1
]).

-spec start(Id :: atom()) -> Ref :: reference().
start(Id) ->
    Ref = erlang:start_timer(?CAST_TIMEOUT, self(), #timer{id = Id}),
    lager:debug("Start timer, Id:~100p, Ref:~100p", [Id, Ref]),
    Ref.

-spec cancel(Ref :: reference()) -> term().
cancel(Ref) ->
    lager:debug("Cancel timer, Ref:~100p", [Ref]),
    erlang:cancel_timer(Ref, [{info, false}]).
