-record(chat_log,   {
            time,       %term()
            name,       %term()
            message     %term()
        }).

-record(chat_waiter,    {
            pid,            %int()
            session_cookie, %term()
            id              %term()
        }).

-record(chat_member,    {
            session_cookie, %term()
            name,           %term()
            access_time     %term()
        }).


