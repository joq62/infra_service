%% Author: uabjle
%% Created: 10 dec 2012
%% Description: TODO: Add description to application_org
-module(infra_service). 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(SERVER,infra_service_server).
%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 is_config/0,
	 config/1,
	 start_orchistrate/0,
	 orchistrate_result/3,
	 ping/0

	]).

-export([
	 start/0,
	 stop/0
	]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------



start()-> gen_server:start_link({local, ?SERVER}, ?SERVER, [], []).
stop()-> gen_server:call(?SERVER, {stop},infinity).


ping() ->
    gen_server:call(?SERVER, {ping},infinity).

is_config()->
    gen_server:call(?SERVER, {is_config},infinity).
config(ClusterSpec)->
    gen_server:call(?SERVER, {config,ClusterSpec},infinity).
    
start_orchistrate()->
    gen_server:call(?SERVER, {start_orchistrate},infinity).
orchistrate_result(ResultStartParentPods,ResultStartInfraAppls,ResultStartUserAppls)->
    gen_server:cast(?SERVER,{orchistrate_result,ResultStartParentPods,
		    ResultStartInfraAppls,ResultStartUserAppls},infinity).    


%% ====================================================================!
%% External functions
%% ====================================================================!


%% ====================================================================
%% Internal functions
%% ====================================================================
