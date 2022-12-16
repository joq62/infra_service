%%%-------------------------------------------------------------------
%% @doc ops_node public API
%% @end
%%%-------------------------------------------------------------------

-module(oam_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    oam_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
