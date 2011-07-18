-module(chat_data).
-export([start/0, init/0]).
-export([member_add/2, member_update_access_time/1, member_get/1]).
-export([message_add/3, message_get/1]).
-export([listener_add/3, listeners_get/0, listeners_clear/0]).
-include("chat.hrl").

-define(CHAT_LOG_SIZE, 100).

%% --------------------------
%% public functions
%% --------------------------
start() ->
    spawn(?MODULE, init, []).

init() ->
    register(?MODULE, self()),
    Table = create_data(chat_data),

    % init data
    insert(Table, log_id, 0),
    insert(Table, log, []),
    insert(Table, member, []),
    insert(Table, listener, []),

    % start loop
    loop(Table).

member_add(UserId, Name) ->
    Member = #chat_member {
        user_id = UserId,
        name = Name,
        access_time = get_now()
    },

    ?MODULE ! {member_add, Member, self()},
    receive
        {reply, ok} -> ok
    end.

member_update_access_time(UserId) ->
    ?MODULE ! {member_update_access_time, UserId, self()},
    receive
        {reply, ok} -> ok
    end.

member_get(UserId) ->
    ?MODULE ! {member_get, UserId, self()},
    receive
        {reply, {ok, Member}} -> Member;
        {reply, {error, _Reason}} -> []
    end.

message_add(UserId, Name, Message) ->
    % get new log id
    ?MODULE ! {increment_log_id, self()},
    NewLogId = receive
        {reply, LogId} -> LogId
    end,
    
    % create chat message record
    MessageRecord = #chat_log{
        log_id = NewLogId,
        time = get_now(),
        user_id = UserId,
        name = Name,
        message = lists:flatten(Message)},

    ?MODULE ! {message_add, MessageRecord, self()},
    receive
        {reply, ok} -> ok
    end.

message_get(MessageId) ->
    ?MODULE ! {message_get, MessageId, self()},
    receive
        {reply, Message} -> Message
    end.

listener_add(UserId, MessageId, ListenerPid) ->
    Listener = #chat_listener {
        user_id = UserId,
        message_id = MessageId,
        pid = ListenerPid
    },
    ?MODULE ! {listener_add, Listener, self()},
    receive
        {reply, ok} -> ok
    end.

listeners_get() ->
    ?MODULE ! {listeners_get, self()},
    receive
        {reply, Listeners} -> Listeners
    end.

listeners_clear() ->
    ?MODULE ! {listeners_clear, self()},
    receive
        {reply, ok} -> ok
    end.

%% --------------------------
%% private functions
%% --------------------------
loop(Table) ->
    receive
        {insert, {Key, Value}, Pid} ->
            insert(Table, Key, Value),
            Pid ! {reply, ok};
        {lookup, Key, Pid} ->
            Value = lookup(Table, Key),
            Pid ! {reply, Value};
        {member_add, Member, Pid} ->
            handle_member_add(Table, Member),
            Pid ! {reply, ok};
        {member_update_access_time, UserId, Pid} ->
            handle_member_update_access_time(Table, UserId),
            Pid ! {reply, ok};
        {member_get, UserId, Pid} ->
            Member = handle_member_get(Table, UserId),
            Pid ! {reply, Member};
        {message_add, Message, Pid} ->
            handle_message_add(Table, Message),
            Pid ! {reply, ok};
        {message_get, MessageId, Pid} ->
            Message = handle_message_get(Table, MessageId),
            Pid ! {reply, Message};
        {listener_add, Listener, Pid} ->
            handle_listener_add(Table, Listener),
            Pid ! {reply, ok};
        {listeners_get, Pid} ->
            Listeners = handle_listeners_get(Table),
            Pid ! {reply, Listeners};
        {listeners_clear, Pid} ->
            handle_listeners_clear(Table),
            Pid ! {reply, ok};
        {increment_log_id, Pid} ->
            LogId = handle_increment_log_id(Table),
            Pid ! {reply, LogId}
    end,

    loop(Table).

handle_member_add(Table, Member) ->
    [{member, Members}] = lookup(Table, member),
    NewMembers = merge_member_list(Member, Members),
    insert(Table, member, NewMembers).

handle_member_update_access_time(Table, UserId) ->
    [{member, Members}] = lookup(Table, member),
    case lists:keysearch(UserId, #chat_member.user_id, Members) of
        {value, #chat_member{user_id = UserId} = Member} -> 
            NewMember = Member#chat_member{access_time = get_now()},
            NewMembers = merge_member_list(NewMember, Members),
            insert(Table, member, NewMembers);
        Else -> Else % ignore if not found
    end.

handle_member_get(Table, UserId) ->
    [{member, Members}] = lookup(Table, member),
    case lists:keysearch(UserId, #chat_member.user_id, Members) of
        {value, Member} -> 
            {ok, Member};
        _Else -> 
            {error, not_found}
    end.

handle_message_add(Table, Message) ->
    [{log, Log}] = lookup(Table, log),
    NewLog = Log ++ [Message],

    % trim log
    TrimmedLog = if
        length(NewLog) > ?CHAT_LOG_SIZE ->
            lists:nthtail(length(Log) - ?CHAT_LOG_SIZE, NewLog);
        true ->
            % default
            NewLog
    end,

    % save log
    insert(Table, log, TrimmedLog).

handle_message_get(Table, MessageId) ->
    [{log, Log}] = lookup(Table, log),
    TrimmedMessage = lists:dropwhile(
        fun(E) -> 
            #chat_log{log_id = LogId} = E,
            LogId =< MessageId
        end, Log),
    TrimmedMessage.

handle_listener_add(Table, Listener) ->
    [{listener, Listeners}] = lookup(Table, listener),
    NewListeners = Listeners ++ [Listener],
    insert(Table, listener, NewListeners).

handle_listeners_get(Table) ->
    [{listener, Listeners}] = lookup(Table, listener),
    Listeners.

handle_listeners_clear(Table) ->
    insert(Table, listener, []).

handle_increment_log_id(Table) ->
    [{log_id, CurrentId}] = lookup(Table, log_id),
    NewId = CurrentId + 1,
    insert(Table, log_id, NewId),
    NewId.

get_now() ->
    calendar:datetime_to_gregorian_seconds(
        calendar:now_to_local_time(now())
    ).

merge_member_list(NewMember, Members) ->
    lists:ukeymerge(
        #chat_member.user_id,
        [NewMember],
        Members
    ).
%% --------------------------
%% data manupilate functions
%% --------------------------
create_data(Name) ->
    ets:new(Name, []).

insert(Table, Key, Value) ->
    ets:insert(Table, {Key, Value}).

lookup(Table, Key) ->
    ets:lookup(Table, Key).
