-record(registration_request, {
    user :: binary(),
    password :: binary()
}).
-type registration_request() :: #registration_request{}.

-record(registration_response, {
    success :: boolean(),
    description :: binary()
}).
-type registration_response() :: #registration_response{}.

-record(authorization_request, {
    user :: binary(),
    password :: binary()
}).
-type authorization_request() :: #authorization_request{}.

-record(authorization_response, {
    success :: boolean(),
    description :: binary()
}).
-type authorization_response() :: #authorization_response{}.

-record(get_users_list_request, {
    offset :: non_neg_integer(), %% 0..
    limit :: pos_integer() %% 1..50
}).
-type get_users_list_request() :: #get_users_list_request{}.

-record(get_users_list_response, {
    success :: boolean(),
    description :: undefined | binary(),
    list :: list(binary())
}).
-type get_users_list_response() :: #get_users_list_response{}.

-type userdb_mysql_manager_request() ::
    registration_request() |
    authorization_request() |
    get_users_list_request().

-type userdb_mysql_manager_response() ::
    registration_response() |
    authorization_response() |
    get_users_list_response().
