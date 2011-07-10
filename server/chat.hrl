-record(chat_log,   {
            log_id,     %term()
            time,       %term()
            user_id,    %term()
            name,       %term()
            message     %term()
        }).

-record(chat_waiter,    {
            pid,            %int()
            user_id,        %term()
            id              %term()
        }).

-record(chat_member,    {
            user_id,        %term()
            name,           %term()
            access_time     %term()
        }).


