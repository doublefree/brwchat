-module(chat_data).
-export([start/0, init/0]).
-export([member_add/2, message_add/3]).
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
    insert(Table, waiter, []),

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

message_add(UserId, Name, Message) ->
    % get new log id
    ?MODULE ! {increment_log_id, self()},
    receive
        {reply, ok} -> ok
    end,
    
    % create chat message record
    MessageRecord = #chat_log{
        time = get_now(),
        user_id = UserId,
        name = Name,
        message = lists:flatten(Message)},

    ?MODULE ! {message_add, MessageRecord, self()},
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
        {message_add, Message, Pid} ->
            handle_message_add(Table, Message),
            Pid ! {reply, ok};
        {increment_log_id, Pid} ->
            handle_increment_log_id(Table),
            Pid ! {reply, ok}
    end,

    loop(Table).

handle_member_add(Table, Member) ->
    [{member, Members}] = lookup(Table, member),
    NewMembers = lists:ukeymerge(
        #chat_member.user_id,
        [Member],
        Members
    ),

    insert(Table, member, NewMembers).

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

handle_increment_log_id(Table) ->
    [{log_id, CurrentId}] = lookup(Table, log_id),
    NewId = CurrentId + 1,
    insert(Table, log_id, NewId),
    NewId.

get_now() ->
    calendar:datetime_to_gregorian_seconds(
        calendar:now_to_local_time(now())
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
