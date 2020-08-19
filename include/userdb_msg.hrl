-type userdb_message_options() :: #{
    from => term(),
    ref => term()
}.

-record(userdb_msg, {
    body :: term(),
    options = #{}:: userdb_message_options()
}).
-type userdb_message() :: #userdb_msg{}.
