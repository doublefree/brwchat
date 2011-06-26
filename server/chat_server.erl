-module(chat_server).
-export([start/0, init/0]).
-include("chat.hrl").
-include("/opt/local/lib/yaws/include/yaws.hrl").

-define(CHATDATA, "data/chat_data").
-define(DETSOPT, [{auto_save, 1000}]).

start() ->
    application:start(inets),
    spawn(?MODULE, init, []).

init() ->
    process_flag(trap_exit, true),
    register(?MODULE, self()),

    %% open dets to store chat data
    dets:open_file(?CHATDATA, ?DETSOPT),
    case dets:lookup(?CHATDATA, logid) of
        [{logid, _}] ->
            %% data initialization is already finished
            true;
        _Else ->
            %% init dets
            dets:insert(?CHATDATA, {logid, 0}),     % Max of logid
            dets:insert(?CHATDATA, {log, []}),      % Log, record "chat_log" 
            dets:insert(?CHATDATA, {waiter, []}),   % Comet waiter, record "chat_waiter" 
            dets:insert(?CHATDATA, {member, []})    % Chat member, record "chat_member"
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
    [{member, Members}] = dets:lookup(?CHATDATA, member),
    Member = #chat_member{
        session_cookie = SessionCookie,
        name = get_unique_name(Members, SessionCookie, Name, 0),
        access_time = get_now()
    },
    dets:insert(?CHATDATA, {
        member,
        lists:ukeymerge(
            #chat_member.session_cookie,
            [Member],
            Members
        )}),
    ok.

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
