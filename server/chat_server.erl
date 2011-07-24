-module(chat_server).
-export([start/0, init/0]).
-export([join/2, get_message/2, post_message/2]).
-include("chat.hrl").
-include("/opt/local/lib/yaws/include/yaws.hrl").

-define(SYSTEM_ID, 0).
-define(SYSTEM_NAME, "system").
-define(MESSAGE_WAIT_SEC, 30).
-define(POLLING_SEC, 15).
-define(MEMBER_TIMEOUT_SEC, 60).

%% --------------------------
%% public functions
%% --------------------------
start() ->
    application:start(inets),
    spawn(?MODULE, init, []).

init() ->
    process_flag(trap_exit, true),
    register(?MODULE, self()),
    chat_data:start(),

    %% start to loop
    loop().

join(SessionIdentifier, Name) ->
    ?MODULE ! {join, SessionIdentifier, Name, self()},
    receive
        {reply, ok} -> ok
    end.

get_message(SessionIdentifier, MessageId) ->
    ?MODULE ! {get_message, SessionIdentifier, MessageId, self()},
    receive
        {reply, Message} -> Message
    after ?MESSAGE_WAIT_SEC * 1000 ->
        % reply empty message
        ?MODULE ! {cancel_message, SessionIdentifier},
        []
    end.

post_message(SessionIdentifier, Message) ->
    ?MODULE ! {post_message, SessionIdentifier, Message, self()},
    receive
        {reply, Status} -> Status
    end.

%% --------------------------
%% private functions
%% --------------------------
loop() ->
    receive
        {join, SessionIdentifier, Name, Pid} ->
            %% join to chat
            handle_join(SessionIdentifier, Name),
            Pid ! {reply, ok};
        {get_message, SessionIdentifier, MessageId, Pid} ->
            %% get chat message
            handle_get_message(SessionIdentifier, MessageId, Pid);
        {post_message, SessionIdentifier, Message, Pid} ->
            %% post chat message
            Status = handle_post_message(SessionIdentifier, Message),
            Pid ! {reply, Status};
        {cancel_message, SessionIdentifier} ->
            %% cancel of "get message"
            handle_cancel_message(SessionIdentifier);
        {_, Pid} ->
            Pid ! {ok, self()}
    after ?POLLING_SEC * 1000 ->
        no_message
    end,

    % call notify when any events occur
    notify(),
    % refresh member list to delete members who leave chat
    purge_members(),

    loop().

%% message handler 
handle_join(SessionIdentifier, Name) ->
    Count = chat_data:member_add(SessionIdentifier, Name),
    Message = io_lib:format("~s joined. total count ~p", [Name, Count]),
    system_message(Message).

handle_get_message(SessionIdentifier, MessageId, Pid) ->
    chat_data:member_update_access_time(SessionIdentifier),
    reply(SessionIdentifier, MessageId, Pid).

handle_post_message(SessionIdentifier, Message) ->
    Member = chat_data:member_get(SessionIdentifier),
    Name = Member#chat_member.name,
    chat_data:message_add(SessionIdentifier, Name, Message).

handle_cancel_message(SessionIdentifier) ->
    chat_data:listener_delete(SessionIdentifier).

%% local functions
reply(SessionIdentifier, MessageId, Pid) ->
    Message = chat_data:message_get(MessageId),
    case length(Message) of
        0 -> chat_data:listener_add(SessionIdentifier, MessageId, Pid);
        _Length -> 
            Pid ! {reply, Message}
    end.

notify() ->
    Listeners = chat_data:listener_list(),
    chat_data:listener_clear_all(),
    lists:foreach(fun(E) ->
        #chat_listener{
            user_id = SessionIdentifier,
            message_id = MessageId,
            pid = Pid
        } = E,
        reply(SessionIdentifier, MessageId, Pid)
    end, Listeners).

purge_members() ->
    MemberList = chat_data:member_list(),
    lists:foreach(fun(Member) ->
        AccessTime = Member#chat_member.access_time,
        Now = chat_util:get_now(),
        case AccessTime + ?MEMBER_TIMEOUT_SEC < Now of
            true ->
                UserId = Member#chat_member.user_id,
                chat_data:member_delete(UserId),
                chat_data:listener_delete(UserId),
                Message = io_lib:format("~s leaved", [Member#chat_member.name]),
                system_message(Message);
            _Else -> _Else
        end
    end, MemberList).

system_message(Message) ->
    chat_data:message_add(?SYSTEM_ID, ?SYSTEM_NAME, Message).
