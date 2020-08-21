-define(TIMER_MAKE_SESSION, timer_make_session).
-define(TIMER_REGISTRATION, timer_registration).
-define(TIMER_AUTHORIZATION, timer_authorization).
-define(TIMER_GET_USERS_LIST, timer_get_users_list).
-type timer_id() ::
    ?TIMER_MAKE_SESSION |
    ?TIMER_REGISTRATION |
    ?TIMER_AUTHORIZATION |
    ?TIMER_GET_USERS_LIST.

-record(timer, {
    id :: timer_id()
}).
-type timer() :: #timer{}.
-define(CAST_TIMEOUT, 1000).
