-define(TIMER_MAKE_SESSION, timer_make_session).
-define(TIMER_REGISTRATION, timer_registration).
-define(TIMER_AUTHORIZATION, timer_authorization).
-type timer_id() ::
    ?TIMER_MAKE_SESSION |
    ?TIMER_REGISTRATION |
    ?TIMER_AUTHORIZATION.

-record(timer, {
    id :: timer_id()
}).
-type timer() :: #timer{}.
-define(CAST_TIMEOUT, 1000).
