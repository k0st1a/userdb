-record(session, {
    id :: non_neg_integer(),
    user :: nonempty_string()
}).
-type session() :: session().
