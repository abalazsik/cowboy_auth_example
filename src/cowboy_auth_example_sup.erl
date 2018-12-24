%%%-------------------------------------------------------------------
%% @doc cowboy_auth_example top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cowboy_auth_example_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) ->
    Config = maps:from_list(application:get_all_env(cowboy_auth_example)),
    {ok, { {one_for_all, 0, 1}, [
        #{
            id => handlers,
            start => {handlers, start, [
                Config
            ]},
            restart => permanent,
            shutdown => 1000,
            type => worker,
            modules => [handlers]
        }
    ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
