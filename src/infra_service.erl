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
	
	 ping/0

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
ping() ->
    gen_server:call(?SERVER, {ping}).

orchistrate_result(ResultStartParentPods,ResultStartInfraAppls,ResultStartUserAppls)->
    gen_server:cast(?SERVER,{orchistrate_result,ResultStartParentPods,
		    ResultStartInfraAppls,ResultStartUserAppls}).    


%% ====================================================================!
%% External functions
%% ====================================================================!


%% ====================================================================
%% Internal functions
%% ====================================================================
