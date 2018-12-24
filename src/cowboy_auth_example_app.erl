%%%-------------------------------------------------------------------
%% @doc cowboy_auth_example public API
%% @end
%%%-------------------------------------------------------------------

-module(cowboy_auth_example_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    cowboy_auth_example_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================