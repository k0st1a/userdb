-module(userdb_mysql_manager_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("userdb_mysql_manager_api.hrl").

handle_test_() ->
    [
        {"Bad user or password in registration_request", [
            ?_assertEqual(
                #registration_response{success = false, description = <<"Bad user or password">>},
                userdb_mysql_manager:handle(#registration_request{}, {state, undefined})
            )
        ]},
        {"Bad user or password in authorization_request", [
            ?_assertEqual(
                #authorization_response{success = false, description = <<"Bad user or password in request">>},
                userdb_mysql_manager:handle(#authorization_request{}, {state, undefined})
            )
        ]},
        {"Bad offset or limit in get_user_list_request", [
            ?_assertEqual(
                #get_users_list_response{success = false, description = <<"Bad offset or limit">>},
                userdb_mysql_manager:handle(#get_users_list_request{}, {state, undefined})
            )
        ]},
        {"Bad user or password or new password in change_user_password_request", [
            ?_assertEqual(
                #change_user_password_response{success = false, description = <<"Bad user or password or new password">>},
                userdb_mysql_manager:handle(#change_user_password_request{}, {state, undefined})
            )
        ]}
    ].

-endif.
