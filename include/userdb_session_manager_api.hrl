-record(add_session_request, {
    session_id :: non_neg_integer(),
    user_name :: binary()
}).
-type add_session_request() :: #add_session_request{}.

-record(add_session_response, {
}).
-type add_session_response() :: #add_session_response{}.

-record(session_filter, {
    match_conditions = []  :: list(),
    match_return = ['$_'] :: list(),
    id = '_' :: '_' | non_neg_integer(),
    user = '_' :: '_' | binary()
}).
-type session_filter() :: #session_filter{}.

-type session_manager_message_body() ::
    add_session_request() |
    add_session_response().

-type session_manager_message_options() :: #{
    from => term(),
    ref => term()
}.

-record(sm_msg, {
    body :: session_manager_message_body(),
    options = #{}:: session_manager_message_options()
}).
-type session_manager_message() :: #sm_msg{}.
