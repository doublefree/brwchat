-module(chat_util).
-export([get_now/0]).

get_now() ->
    calendar:datetime_to_gregorian_seconds(
        calendar:now_to_local_time(now())
    ).
