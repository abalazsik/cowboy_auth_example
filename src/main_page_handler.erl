-module(main_page_handler).

-export([init/2, content_types_provided/2, serve/2]).

-behaviour(cowboy_rest).

init(Req, State) ->
    {cowboy_rest, Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"text/html">>, serve}
	], Req, State}.

serve(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            Session = util:getSessionData(Req),

                Response = [
                <<"<html>
                    <head><title>Welcome!</title></head>
                    <body><div>
                        <a style=\"float:right\" href=\"\\logout\">logout</a>
                        <h1>Welcome ">>, sessiondata:getUsername(Session) ,<<"!</h1>">>,
                        roles(sessiondata:getRoles(Session)),
                    <<"</div></body>
                </html>">>],
                {Response, Req, State};
        <<"OPTIONS">> ->
            {true, Req, State};
        _ ->
            {stop, Req, State}
    end.

roles(Roles) ->
    [<<"<div>Your roles: ">>, <<"<ul>">>,
        [[<<"<li>">>, erlang:atom_to_binary(Role,unicode), <<"</li>">>] || Role <- Roles],
    <<"</ul></div>">>].
