-module(chat_server).
-export([start/0, init/0]).
-include("chat.hrl").
-include("/opt/local/lib/yaws/include/yaws.hrl").

-define(CHAT_LOG_SIZE, 100).
-define(CHAT_DATA, "data/chat_data").
-define(DETS_OPT, [{auto_save, 1000}]).

start() ->
    application:start(inets),
    spawn(?MODULE, init, []).

init() ->
    process_flag(trap_exit, true),
    register(?MODULE, self()),

    %% open dets to store chat data
    dets:open_file(?CHAT_DATA, ?DETS_OPT),
    case dets:lookup(?CHAT_DATA, log_id) of
        [{log_id, _}] ->
            %% data initialization is already finished
            true;
        _Else ->
            %% init dets
            dets:insert(?CHAT_DATA, {log_id, 0}),    % current log_id
            dets:insert(?CHAT_DATA, {log, []}),      % Log, record "chat_log" 
            dets:insert(?CHAT_DATA, {waiter, []}),   % Comet waiter, record "chat_waiter" 
            dets:insert(?CHAT_DATA, {member, []})    % Chat member, record "chat_member"
    end,

    %% start to loop
    loop().

loop() ->
    receive
        {join, SessionCookie, Name} ->
            %% join to chat
            join(SessionCookie, Name);
        {_, Pid} ->
            Pid ! {ok, self()}
    end,

    loop().

join(SessionCookie, Name) ->
    [{member, Members}] = dets:lookup(?CHAT_DATA, member),
    Member = #chat_member{
        session_cookie = SessionCookie,
        name = get_unique_name(Members, SessionCookie, Name, 0),
        access_time = get_now()
    },
    NewMembers = lists:ukeymerge(
            #chat_member.session_cookie,
            [Member],
            Members
        ),
    dets:insert(?CHAT_DATA, {member, NewMembers}),

    Message = io_lib:format("~s joined", [Name]),
    add_message("System", Message).

get_unique_name(Members, SessionCookie, Name, SynCount) ->
    GeneratedName = generate_name(Name, SynCount),
    FoundSame = lists:any(
        fun(#chat_member{
                session_cookie = UserSessionCookie,
                name = UserName}) ->
            (UserName == GeneratedName)
            and (UserSessionCookie /= SessionCookie)
        end, Members),
    case FoundSame of
        true -> get_unique_name(Members, SessionCookie, Name, SynCount+1);
        false -> GeneratedName
    end.

generate_name(Name, SynCount) ->
    case SynCount of
        0 -> Name;
        _ -> Name ++ "#" ++ integer_to_list(SynCount)
    end.

get_now() ->
    calendar:datetime_to_gregorian_seconds(
        calendar:now_to_local_time(now())
    ).

add_message(Name, Message) ->
    % increment log id
    [{log_id, CurrentId}] = dets:lookup(?CHAT_DATA, log_id),
    NewId = CurrentId + 1,
    dets:insert(?CHAT_DATA, {log_id, NewId}),

    % lookup chat log
    [{log, Log}] = dets:lookup(?CHAT_DATA, log),

    % new chat record
    {{_Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(now()),
    Time = lists:flatten(io_lib:format("~w/~w ~2.2.0w:~2.2.0w:~2.2.0w", [Month, Day, Hour, Minute, Second])),
    NewRecord = #chat_log{
        time = Time,
        name = Name,
        message = lists:flatten(Message)},
    NewLog = Log ++ [NewRecord],

    % trim log
    TrimmedLog = if
        length(NewLog) > ?CHAT_LOG_SIZE ->
            lists:nthtail(length(Log) - ?CHAT_LOG_SIZE, NewLog);
        true ->
            % default
            NewLog
    end,

    % save log
    dets:insert(?CHAT_DATA, {log, TrimmedLog}).
