%%%-------------------------------------------------------------------
%% @doc test_area public API
%% @end
%%%-------------------------------------------------------------------

-module(test_area_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    test_area_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
