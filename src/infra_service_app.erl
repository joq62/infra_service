%%%-------------------------------------------------------------------
%% @doc org public API
%% @end
%%%-------------------------------------------------------------------

-module(infra_service_app).

-behaviour(application).

-export([start/2, stop/1]). 

start(_StartType, _StartArgs) ->
    infra_service_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
