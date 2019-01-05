-module(auth_middleware).

-export([execute/2, getCookieName/0]).

%%this middleware checks wheter the user is authenticated. If not, it redirects to the login page

-behaviour(cowboy_middleware).

-define(COOKIE, <<"sessionid">>).

execute(Req, Env) ->
    case isPublic(cowboy_req:path(Req)) of
      true ->
        {ok, Req, Env};
      false ->
        case util:getSessionData(Req) of
            invalid ->
                util:redirectAsResponse("/login.html", Req);
            _->
                {ok, Req, Env}
        end
    end.

-spec isPublic(binary()) -> boolean().

%% if true the theres is no authentication on the page/endpoint

isPublic(<<"/login">>) -> true;
isPublic(<<"/login.html">>) -> true;
isPublic(_) -> false.

getCookieName() ->
    ?COOKIE.
