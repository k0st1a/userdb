-record(registration_request, {
    user :: binary(),
    password :: binary()
}).
-type registration_request() :: #registration_request{}.

-record(registration_response, {
    success :: boolean(),
    description :: undefined | binary()
}).
-type registration_response() :: #registration_response{}.

-record(authorization_request, {
    user :: binary(),
    password :: binary()
}).
-type authorization_request() :: #authorization_request{}.

-record(authorization_response, {
    success :: boolean(),
    description :: undefined | binary()
}).
-type authorization_response() :: #authorization_response{}.

-type userdb_mysql_manager_request() ::
    registration_request() |
    authorization_request().

-type userdb_mysql_manager_response() ::
    registration_response() |
    authorization_response().
