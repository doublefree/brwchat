-module(chat_server).
-export([start/0, init/0]).
-export([join/2]).
-include("chat.hrl").
-include("/opt/local/lib/yaws/include/yaws.hrl").

-define(SYSTEM_ID, 0).
-define(SYSTEM_NAME, 0).

start() ->
    application:start(inets),
    spawn(?MODULE, init, []).

init() ->
    process_flag(trap_exit, true),
    register(?MODULE, self()),
    chat_data:start(),

    %% start to loop
    loop().

join(SessionCookie, Name) ->
    ?MODULE ! {join, SessionCookie, Name}.

loop() ->
    receive
        {join, SessionCookie, Name} ->
            %% join to chat
            handle_join(SessionCookie, Name);
        {_, Pid} ->
            Pid ! {ok, self()}
    end,

    loop().

handle_join(SessionCookie, Name) ->
    chat_data:member_add(SessionCookie, Name),

    Message = io_lib:format("~s joined", [Name]),
    chat_data:message_add(?SYSTEM_ID, ?SYSTEM_NAME, Message).
