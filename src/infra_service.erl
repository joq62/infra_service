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
    gen_server:call(?SERVER, {ping}).

is_config()->
    gen_server:call(?SERVER, {is_config}).
config(ClusterSpec)->
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DEBUG config,ClusterSpec  : ",config,ClusterSpec,?MODULE,?LINE]]),
    gen_server:call(?SERVER, {config,ClusterSpec}).
    

orchistrate_result(ResultStartParentPods,ResultStartInfraAppls,ResultStartUserAppls)->
    gen_server:cast(?SERVER,{orchistrate_result,ResultStartParentPods,
		    ResultStartInfraAppls,ResultStartUserAppls}).    


%% ====================================================================!
%% External functions
%% ====================================================================!


%% ====================================================================
%% Internal functions
%% ====================================================================
