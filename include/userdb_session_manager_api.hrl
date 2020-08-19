-record(make_session_request, {
    user_name :: binary()
}).
-type make_session_request() :: #make_session_request{}.

-record(make_session_response, {
    session_id :: reference()
}).
-type make_session_response() :: #make_session_response{}.

-record(session_filter, {
    match_conditions = []  :: list(),
    match_return = ['$_'] :: list(),
    id = '_' :: '_' | non_neg_integer(),
    user = '_' :: '_' | binary()
}).
-type session_filter() :: #session_filter{}.

-type session_manager_message_body() ::
    make_session_request() |
    make_session_response().

-type session_manager_message_options() :: #{
    from => term(),
    ref => term()
}.

-record(sm_msg, {
    body :: session_manager_message_body(),
    options = #{}:: session_manager_message_options()
}).
-type session_manager_message() :: #sm_msg{}.
