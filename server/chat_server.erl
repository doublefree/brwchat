-module(chat_server).
-export([start/0, init/0]).
-include("/opt/local/lib/yaws/include/yaws.hrl").

-define(DETSOPT, [{auto_save, 1000}]).

start() ->
    application:start(inets),
    spawn(?MODULE, init, []).

init() ->
    process_flag(trap_exit, true),
    register(?MODULE, self()),

    %% open depts to store chat data
    dets:open_file("data/chat_data", ?DETSOPT),
    loop().

loop() ->
    receive
        {_, Pid} ->
            Pid ! {ok, self()}
    end,

    loop().
