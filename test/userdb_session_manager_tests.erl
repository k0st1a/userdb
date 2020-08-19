-module(userdb_session_manager_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").
-include("userdb_session_manager_api.hrl").
-include("userdb_msg.hrl").
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
            {"Check make_session_request call and find", fun () ->
                Msg =
                    userdb_session_manager:call(
                        #userdb_msg{
                            body = #make_session_request{
                                user_name = <<"12345 user name">>
                            }
                        }
                    ),
                ?assertMatch(
                    #userdb_msg{body=#make_session_response{}},
                    Msg
                ),
                ?assertEqual(
                    [<<"12345 user name">>],
                    userdb_session_manager:find_session(Msg#userdb_msg.body#make_session_response.session_id)
                )
            end},
            {"Check make_session_request cast and find", fun () ->
                Ref = userdb_session_manager:cast(#make_session_request{user_name = <<"123456 user name">>}),
                receive
                    #userdb_msg{body=#make_session_response{}, options = #{ref := Ref}} = Msg ->
                        ?assertEqual(
                            [<<"123456 user name">>],
                            userdb_session_manager:find_session(Msg#userdb_msg.body#make_session_response.session_id)
                        );
                    Msg ->
                        throw({msg, Msg})
                after
                    100 ->
                        throw({timeout, 100})
                end
            end},
            {"Check find", fun () ->
                ?assertEqual(
                    [
                        <<"12345 user name">>,
                        <<"123456 user name">>
                    ],
                    userdb_session_manager:find(#session_filter{user = '$1', match_return = ['$1']})
                )
            end}
        ]
    }
].

-endif.
