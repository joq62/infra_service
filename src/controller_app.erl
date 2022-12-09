%%%-------------------------------------------------------------------
%% @doc ops_node public API
%% @end
%%%-------------------------------------------------------------------

-module(controller_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    controller_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
