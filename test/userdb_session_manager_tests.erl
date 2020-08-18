-module(userdb_session_manager_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("userdb_session_manager_api.hrl").
-include("userdb_session.hrl").

userdb_session_manager_test_() -> [
    {
        setup,
        fun () ->
            userdb:start()
        end,
        fun (_) ->
            userdb:stop()
        end,
        [
            {"Check add_session_request call", fun () ->
                Result =
                    userdb_session_manager:call(
                        #sm_msg{
                            body = #add_session_request{
                                session_id = 12345,
                                user_name = <<"12345 user name">>
                            }
                        }
                    ),
                ?assertEqual(
                    #sm_msg{body=#add_session_response{}},
                    Result
                )
            end},
            {"Check add_session_request cast", fun () ->
                Ref = erlang:make_ref(),
                Self = erlang:self(),
                userdb_session_manager:cast(
                    #sm_msg{
                        body = #add_session_request{
                            session_id = 123456,
                            user_name = <<"123456 user name">>
                        },
                        options = #{
                            src => Self,
                            ref => Ref
                        }
                    }
                ),
                receive
                    #sm_msg{body=#add_session_response{}, options = #{ref := Ref}} ->
                        ok;
                    Msg ->
                        throw({msg,Msg})
                after
                    100 ->
                        throw({timeout, 100})
                end
            end},
            {"Check find", fun () ->
                ?assertEqual(
                    [
                        #session{id = 12345, user = <<"12345 user name">>},
                        #session{id = 123456, user = <<"123456 user name">>}
                    ],
                    userdb_session_manager:find(#session_filter{})
                )
            end}
        ]
    }
].

-endif.
