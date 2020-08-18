-module(userdb_tests).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

userdb_test_() -> [
    {
        setup,
        fun () ->
            userdb:debug(),
            userdb:start(),
            inets:start()
        end,
        fun (_) ->
            inets:stop(),
            userdb:stop(),
            userdb:nodebug()
        end,
        [
            {"Check registration request", fun () ->
                {ok, Data} = file:read_file("./example/registration.json"),
                ?assertEqual(
                    {ok, {200, "{\"description\":\"Well-formed registration request\"}"}},
                    httpc:request(post, {"http://localhost:8080", [], "application/json", Data}, [], [{full_result, false}])
                )
            end},
            {"Check authorization request", fun () ->
                {ok, Data} = file:read_file("./example/authorization.json"),
                ?assertEqual(
                    {ok, {200, "{\"description\":\"Well-formed authorization request\"}"}},
                    httpc:request(post, {"http://localhost:8080", [], "application/json", Data}, [], [{full_result, false}])
                )
            end},
            {"Check change_user_password request", fun () ->
                {ok, Data} = file:read_file("./example/changer_user_password.json"),
                ?assertEqual(
                    {ok, {200, "{\"description\":\"Well-formed change_user_password request\"}"}},
                    httpc:request(post, {"http://localhost:8080", [], "application/json", Data}, [], [{full_result, false}])
                )
            end},
            {"Check get_user_list request", fun () ->
                {ok, Data} = file:read_file("./example/get_users_list.json"),
                ?assertEqual(
                    {ok, {200, "{\"description\":\"Well-formed get_users_list request\"}"}},
                    httpc:request(post, {"http://localhost:8080", [], "application/json", Data}, [], [{full_result, false}])
                )
            end}
        ]
    }
].

-endif.
