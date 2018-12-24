-module(role2_middleware).

-export([execute/2]).

%%this middleware checks wheter the user wants to access the static role2.html file and has role2. If not, it throws 403

-behaviour(cowboy_middleware).

-define(COOKIE, <<"sessionid">>).

execute(Req, Env) ->
    case cowboy_req:path(Req) of
        <<"/role2.html">> ->
            Session = util:getSessionData(Req),
            case sessiondata:hasRole(Session, role2) of
                true -> {ok, Req, Env};
                _ -> util:forbidden(Req)
            end;
        _ ->
            {ok, Req, Env}
    end.
