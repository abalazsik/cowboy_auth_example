-module(managers_only_handler).

-export([init/2, content_types_provided/2, forbidden/2, serve/2]).

-behaviour(cowboy_rest).

init(Req, State) ->
    {cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"text/html">>, serve}
	], Req, State}.

forbidden(Req, State) ->
    Session = util:getSessionData(Req),
    {(not sessiondata:hasRole(Session,manager)), Req, State}.

serve(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            Response = 
            <<"<html>
                <head><title>Managers only</title></head>
                <body><div>Only managers can see this page</div></body>
            </html>">>,
            {Response, Req, State};
        _ ->
            {stop, Req, State}
    end.