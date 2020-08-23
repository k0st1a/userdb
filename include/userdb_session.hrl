-record(session, {
    id :: binary(),
    user :: binary(),
    expires :: non_neg_integer()
}).
-type session() :: session().
