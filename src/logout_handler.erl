-module(logout_handler).

-export([init/2]).

-behaviour(cowboy_handler).

init(Req, State) ->
    {Ip, _} = cowboy_req:peer(Req),
    websession:invalidate(util:getCookie(Req,
					 auth_middleware:getCookieName(),
					 undefined),
			  Ip),
    Req1 =
	cowboy_req:set_resp_cookie(auth_middleware:getCookieName(),
				   <<>>, Req),
    {ok, util:redirect("/login.html", Req1), State}.
