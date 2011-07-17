-module(chat_server).
-export([start/0, init/0]).
-export([join/2, get_message/2, post_message/2]).
-include("chat.hrl").
-include("/opt/local/lib/yaws/include/yaws.hrl").

-define(SYSTEM_ID, 0).
-define(SYSTEM_NAME, "system").

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
        {_, Pid} ->
            Pid ! {ok, self()}
    end,

    % call notify when any events occur
    notify(),

    loop().

handle_join(SessionIdentifier, Name) ->
    chat_data:member_add(SessionIdentifier, Name),

    Message = io_lib:format("~s joined", [Name]),
    chat_data:message_add(?SYSTEM_ID, ?SYSTEM_NAME, Message).

handle_get_message(SessionIdentifier, MessageId, Pid) ->
    chat_data:member_update_access_time(SessionIdentifier),
    Message = chat_data:message_get(MessageId),
    case length(Message) of
        0 -> chat_data:listener_add(SessionIdentifier, MessageId, Pid);
        _Length -> Pid ! {reply, Message}
    end.

handle_post_message(SessionIdentifier, Message) ->
    Member = chat_data:member_get(SessionIdentifier),
    Name = Member#chat_member.name,
    chat_data:message_add(SessionIdentifier, Name, Message).

notify() ->
    Listeners = chat_data:listeners_get(),
    chat_data:listeners_clear(),
    lists:foreach(fun(E) ->
        #chat_listener{
            user_id = SessionIdentifier,
            message_id = MessageId,
            pid = Pid
        } = E,
        handle_get_message(SessionIdentifier, MessageId, Pid)
    end, Listeners).
