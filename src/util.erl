-module(util).
-export([
	redirectAsResponse/2,
	redirectAsResponse/3,
	redirect/2,
	forbidden/1,
	getCookie/3,
	getSessionData/1
]).

redirect(ToUrl, Req) ->
	Req1 = cowboy_req:set_resp_header(<<"location">>, ToUrl, Req),
	cowboy_req:reply(303, Req1).

%%for middlewares
redirectAsResponse(ToUrl, Req) ->
	{stop, redirect(ToUrl,Req)}.

%%for handlers
redirectAsResponse(ToUrl, Req, State) ->
	{stop, redirect(ToUrl,Req), State}.

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