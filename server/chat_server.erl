-module(chat_server).
-export([start/0, init/0]).
-export([join/2, get_message/2]).
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
            Message = handle_get_message(SessionIdentifier, MessageId),
            Pid ! {reply, Message}; 
        {_, Pid} ->
            Pid ! {ok, self()}
    end,

    loop().

handle_join(SessionIdentifier, Name) ->
    chat_data:member_add(SessionIdentifier, Name),

    Message = io_lib:format("~s joined", [Name]),
    chat_data:message_add(?SYSTEM_ID, ?SYSTEM_NAME, Message).

handle_get_message(SessionIdentifier, MessageId) ->
    chat_data:member_update_access_time(SessionIdentifier),
    chat_data:message_get(MessageId).
