-module(sessiondata).

-export([new/5, getSessionId/1, getUsername/1, getIp/1, getValidUntil/1, getRoles/1, renewSession/2, hasRole/2]).

%%sessiondata object

-record(sessiondata, {
    sessionid = [] :: binary(),
    username = [] :: string(),
    ip = [] :: inet:ip_address(),
    validUntil = {{0,0,0},{0,0,0}} :: calendar:datetime(),
    roles :: [atom()]
}).

new(Sessionid, Username, Ip, SessionTimeout, Roles) ->
    #sessiondata{
        sessionid = Sessionid,
        username = Username,
        ip = Ip,
        validUntil = iso8601:add_time(erlang:localtime(), 0, SessionTimeout, 0),
        roles = Roles
    }.

getSessionId(SessionData) when is_record(SessionData,sessiondata) ->
    SessionData#sessiondata.sessionid.

getUsername(SessionData) when is_record(SessionData,sessiondata) ->
    SessionData#sessiondata.username.

getIp(SessionData) when is_record(SessionData,sessiondata) ->
    SessionData#sessiondata.ip.

getValidUntil(SessionData) when is_record(SessionData,sessiondata) ->
    SessionData#sessiondata.validUntil.

getRoles(SessionData) when is_record(SessionData,sessiondata) ->
    SessionData#sessiondata.roles.

renewSession(SessionData, SessionTimeout) when is_record(SessionData,sessiondata) andalso SessionTimeout > 0 -> 
    NewValidUntil = iso8601:add_time(erlang:localtime(), 0, SessionTimeout, 0),
    SessionData#sessiondata{validUntil = NewValidUntil}.

hasRole(SessionData, Role) when is_record(SessionData, sessiondata) andalso is_atom(Role) ->
    lists:member(Role, SessionData#sessiondata.roles).
