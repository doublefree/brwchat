-record(chat_log,   {
            log_id,     %term()
            time,       %term()
            user_id,    %term()
            name,       %term()
            message     %term()
        }).

-record(chat_listener,    {
            user_id,        %term()
            message_id,     %term()
            pid             %int()
        }).

-record(chat_member,    {
            user_id,        %term()
            name,           %term()
            access_time     %term()
        }).


