-module(login_handler).

-export([init/2, content_types_accepted/2, allowed_methods/2, serve/2]).

-behaviour(cowboy_rest).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
	{[<<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
    {[{{<<"application">>, <<"x-www-form-urlencoded">>, []}, serve}], Req, State}.

serve(Req, State) ->
    case cowboy_req:method(Req) of
        <<"POST">> ->
            {ok, Body, _ } = cowboy_req:read_urlencoded_body(Req),
            Username = proplists:get_value(<<"username">>, Body, undefined),
            Password = proplists:get_value(<<"password">>, Body, undefined),
            {Ip, _} = cowboy_req:peer(Req),
            case websession:authenticate(Username,Password,Ip) of
                authentication_failed ->
                    util:redirect("/login.html", Req, State);
                Session ->
                    SessionId = sessiondata:getSessionId(Session),
                    Req1 = cowboy_req:set_resp_cookie(auth_middleware:getCookieName(), SessionId, Req, #{max_age => 3600}),
                    util:redirect("/", Req1, State)
            end;
        <<"OPTIONS">> ->
            {true, Req, State};
        _ ->
            {stop, Req, State}
    end.