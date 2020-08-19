-record(registration_request, {
    user :: binary()
}).

-record(registration_response, {
    status :: boolean(),
    description :: binary()
}).
