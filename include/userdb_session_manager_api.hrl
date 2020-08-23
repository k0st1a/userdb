-record(session_filter, {
    match_conditions = []  :: list(),
    match_return = ['$_'] :: list(),
    id = '_' :: '_' | non_neg_integer(),
    user = '_' :: '_' | binary(),
    expires = '_' :: '_' | erlang:timestamp()
}).
-type session_filter() :: #session_filter{}.

-record(make_session_request, {
    user_name :: binary()
}).
-type make_session_request() :: #make_session_request{}.

-record(make_session_response, {
    session_id :: reference()
}).
-type make_session_response() :: #make_session_response{}.
