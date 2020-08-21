-module(userdb_msg).

-include("userdb_msg.hrl").

-export([
    cast/2,
    reply/2
]).

-spec cast(Server :: term(), Body :: term()) -> Ref :: reference().
cast(Server, Body) ->
    Ref = erlang:make_ref(),
    gen_server:cast(Server, #userdb_msg{body = Body, options = #{src => erlang:self(), ref => Ref}}),
    Ref.

-spec reply(Msg :: userdb_message(), Body :: term()) -> boolean().
reply(#userdb_msg{options = #{src := Src, ref := Ref}}, Body) ->
    erlang:send(Src, #userdb_msg{body = Body, options = #{ref => Ref}}),
    true;
reply(_, _) ->
    false.

