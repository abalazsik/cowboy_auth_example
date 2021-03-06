-module(websession).
-export([start/1, start_link/1,init/1,terminate/0,handle_call/3]).
-export([authenticate/3, lookup/2, invalidate/2, flush/0]).

%% sessionhandler

-define(WEBSESSION,websession).
-define(WEBSESSION_TABLE,websession_table).
-define(SESSIONTIMEOUT,30).
-define(OPTIMEOUT,500).
-define(FLUSHINTERVAL,3600).
-define(FLUSHTHREAD,flushthread).

start(Config)->
    start_link(Config).

start_link(Config)-> 
    gen_server:start_link({local,?MODULE},?MODULE, Config, []).

init(_) ->
    ets:new(?WEBSESSION_TABLE,[named_table,set,{keypos,2}]), %%lookup by sessionid
    PID = erlang:spawn( %%this thread will flush every invalid sessiondata from memory every FLUSHINTERVAL seconds
        fun FlushThread() ->
            receive 
                {stop} -> ok
            after ?FLUSHINTERVAL * 1000 ->
                websession:flush(),
                FlushThread()
            end
        end
    ),
    erlang:register(?FLUSHTHREAD,PID),
    {ok,{}}.

terminate() ->
    true = ets:delete(?WEBSESSION_TABLE),ok.

handle_call(Request, _, _) ->
    case Request of
        {lookup, SessionId, Ip} ->
            case ets:lookup(?WEBSESSION_TABLE, SessionId) of %%check if the user is already logged in
                [SessionData] -> %%it is
                    case isValid(
                        sessiondata:getValidUntil(SessionData),
                        sessiondata:getIp(SessionData),
                        Ip) of
                        true -> %%session valid
                            Updated = sessiondata:renewSession(SessionData,?SESSIONTIMEOUT),
                            ets:insert(?WEBSESSION_TABLE,Updated),
                            {reply, Updated, ok};
                        false -> %%session invalid, or expired
                            {reply, invalid, ok}
                        end;
                [] -> %%not logged in
                    {reply, invalid, ok}
            end;
        {authenticate, Username, Password, Ip} ->
            case user_service:authenticateUser(Username,Password) of
                {true, User} ->
                    Roles = appuser:getRoles(User),
                    Response = createSession(Username, Ip, Roles),
                    {reply, Response, ok};
                false ->
                    {reply, authentication_failed, ok}
            end;
        {invalidate, SessionId, Ip} ->
            case ets:lookup(?WEBSESSION_TABLE, SessionId) of %%check if the user is logged in
                [SessionData] -> %%it is
                    case isValid(
                        sessiondata:getValidUntil(SessionData),
                        sessiondata:getIp(SessionData),
                        Ip) of
                        true -> %%session valid
                            ets:delete(?WEBSESSION_TABLE, SessionId),
                            {reply, invalidated, ok};
                        false -> %%session invalid, or expired
                            {reply, invalid, ok}
                        end;
                [] -> %%not logged in
                    {reply, invalid, ok}
            end;
        {flush} ->
            Timestamp = erlang:localtime(),
            N = ets:foldl(
                fun(SessionData, M) ->
                    case sessiondata:getValidUntil(SessionData) < Timestamp of
                        true -> ets:delete(?WEBSESSION_TABLE, sessiondata:getSessionId(SessionData)), M + 1;
                        false -> M
                    end
                end,
                0,
                ?WEBSESSION_TABLE),
            io:format("Flushed ~w sessiondata~n",[N]);
        _ ->
			{reply, unknown_command}
    end.

-spec isValid(
    calendar:datetime(),
    inet:ip_address(),
    inet:ip_address()
) -> boolean().

%%for a production application it would be more secure to check more properties (eg. user-agent)

isValid(ValidUntil, StoredIp, Ip) ->
    Datetime = erlang:localtime(),
    case ValidUntil > Datetime of
        true -> (StoredIp == Ip);
        false  -> false
    end.

createSession(Username, Ip, Roles) -> 
    Sessionid = base64:encode(crypto:strong_rand_bytes(32)),
    SessionData = sessiondata:new(Sessionid, Username, Ip, ?SESSIONTIMEOUT, Roles),
    ets:insert(?WEBSESSION_TABLE,SessionData),
    SessionData.

%%authenticate user

-spec authenticate(
        string() | binary(),
        string() | binary(),
        inet:ip_address()
    ) -> binary() | authentication_failed.

authenticate(Username, Password, Ip) when is_binary(Username) andalso is_binary(Password) ->
    authenticate(erlang:binary_to_list(Username),erlang:binary_to_list(Password), Ip);
authenticate(Username, Password, Ip) when is_list(Username) andalso is_list(Password) -> 
    gen_server:call(?MODULE,{authenticate,Username,Password,Ip},?OPTIMEOUT).

%%lookup sessiondata by sessionid and ip

lookup(undefined, _) ->
    invalid;
lookup(SessionId, Ip) -> 
    gen_server:call(?MODULE,{lookup,SessionId,Ip},?OPTIMEOUT).

%%invalidate session

invalidate(undefined, _) ->
    invalid;
invalidate(SessionId, Ip) ->
    gen_server:call(?MODULE,{invalidate,SessionId,Ip},?OPTIMEOUT).

%%flush invalid sessiondata from the memory

flush() ->
    gen_server:call(?MODULE,{flush},?OPTIMEOUT).