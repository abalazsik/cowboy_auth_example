-module(util).
-export([
	redirect/3,
	redirect/2,
	forbidden/1,
	getSessionData/1
]).

%%for handlers
redirect(ToUrl, Req, State) ->
	Req1 = cowboy_req:set_resp_header(<<"location">>, ToUrl, Req),
	{stop, cowboy_req:reply(303, Req1), State}.

%%for middlewares
redirect(ToUrl, Req) ->
	Req1 = cowboy_req:set_resp_header(<<"location">>, ToUrl, Req),
	{stop, cowboy_req:reply(303, Req1)}.

%%for middlewares
forbidden(Req) ->
	Body = <<"<html><body><h1>403 Forbidden</h1></body></html>">>,
	Req1 = cowboy_req:set_resp_body(Body,Req),
	{stop, cowboy_req:reply(403, Req1)}.

getCookie(Req, Cookie, Default) ->
    Cookies = cowboy_req:parse_cookies(Req),
    proplists:get_value(Cookie, Cookies, Default).

getSessionData(Req) ->
	SessionId = getCookie(Req,auth_middleware:getCookieName(),undefined),
	{Ip, _} = cowboy_req:peer(Req),
	websession:lookup(SessionId,Ip).