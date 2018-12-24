-module(handlers).
-export([init/1, start/1, terminate/0]).

-behaviour(gen_server).

start(Config) ->
    gen_server:start_link({local,?MODULE},?MODULE, Config, []).

init(Config) when is_map(Config) ->
    Port = maps:get(port, Config),
    
    websession:start_link(Config),

    Dispatch = cowboy_router:compile([{'_',[
        {"/login", login_handler,#{}},
        {"/login.html", cowboy_static, {priv_file, cowboy_auth_example, "login.html"}},
        {"/role2.html", cowboy_static, {priv_file, cowboy_auth_example, "role2.html"}},
        {"/", main_page_handler, #{}},
        {"/managers", managers_only_handler, #{}},
        {'_', login_handler, #{}} %% useful if the frontend is a SPA
    ]}]),
    {ok, _} = cowboy:start_clear(my_http_listener,
		[{port, Port}],
		    #{
                env => #{dispatch => Dispatch},
                middlewares => [cowboy_router, auth_middleware, role2_middleware, cowboy_handler]
            }),
	{ok,{}}.

terminate() ->
	ok.