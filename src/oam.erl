%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(oam).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

-define(SERVER,oam_server).

-export([
	 new_appl/2,
	 new_appl/3,
	 update_appl/3,
	 delete_appl/2,
	 deploy_appls/0,
	 
	 is_cluster_deployed/0,
	 ping_connect_nodes/0,

	 new_db_info/0,
	 new_connect_nodes/0,
	 new_controllers/0,
	 new_workers/0,
	 

	 ping/0
	]).

%debug
-export([
	 all_apps/0,
	 present_apps/0,
	 missing_apps/0,
	 call/5,
	 where_is_app/1
	]).

%% ====================================================================
%% External functions
%% ====================================================================

	    
%% call
new_appl(ApplSpec,HostSpec)->
    gen_server:call(?SERVER, {new_appl,ApplSpec,HostSpec},infinity).
new_appl(ApplSpec,HostSpec,TimeOut)->
    gen_server:call(?SERVER, {new_appl,ApplSpec,HostSpec,TimeOut},infinity).
update_appl(ApplSpec,PodNode,HostSpec)->
    gen_server:call(?SERVER, {update_appl,ApplSpec,PodNode,HostSpec},infinity).
delete_appl(ApplSpec,PodNode)->
    gen_server:call(?SERVER, {delete_appl,ApplSpec,PodNode},infinity).
deploy_appls()->
    gen_server:call(?SERVER, {deploy_appls},infinity).


new_db_info()->
    gen_server:call(?SERVER, {new_db_info},infinity).
new_connect_nodes()->
    gen_server:call(?SERVER, {new_connect_nodes},infinity).
new_controllers()->
    gen_server:call(?SERVER, {new_controllers},infinity).
new_workers()->
    gen_server:call(?SERVER, {new_workers},infinity).
	 

is_cluster_deployed()->
    gen_server:call(?SERVER, {is_cluster_deployed},infinity).

%% debug

all_apps()->
    gen_server:call(?SERVER, {all_apps},infinity).

present_apps() ->
    gen_server:call(?SERVER, {present_apps}).

missing_apps() ->
    gen_server:call(?SERVER, {missing_apps}).

where_is_app(App)->
    gen_server:call(?SERVER, {where_is_app,App},infinity).
call(PodNode,M,F,A,T)->
    gen_server:call(?SERVER, {call,PodNode,M,F,A,T},infinity).

ping_connect_nodes()->
    gen_server:call(?SERVER, {ping_connect_nodes},infinity).

ping() ->
    gen_server:call(?SERVER, {ping}).
%% cast


%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
