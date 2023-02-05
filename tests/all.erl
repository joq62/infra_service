%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(all).      
    
 
-export([start/1,
	 print/2]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
print(Arg1,Arg2)->
    io:format(Arg1,Arg2).
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start([ClusterSpec,HostSpec])->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    ok=setup(ClusterSpec),
    ok=infra_service_test:setup(),
    ok=infra_service_test:start_local_appls(ClusterSpec),
    ok=infra_service_test:initiate_local_dbase(ClusterSpec),
    ok=infra_service_test:ensure_right_cookie(ClusterSpec),
    %%-- Start parents
    {ok,ActiveParents}=infra_service_test:start_parents(),
    io:format("ActiveParents !!! ~p~n",[{ActiveParents,?MODULE,?FUNCTION_NAME}]),
    %%-- create pods
    [{ok,NodelogPod,NodelogApplSpec}]=lib_infra_service:create_pods_based_appl("nodelog"),
    [{ok,DbPod,DbApplSpec}]=lib_infra_service:create_pods_based_appl("db_etcd"),
    [{ok,InfraPod,InfraApplSpec}]=lib_infra_service:create_pods_based_appl("infra_service"),
    %%-- create nodelog appl
   % glurk=rpc:call(NodelogPod,application,which_applications,[],5000),
    [{ok,_,_,_}]=lib_infra_service:create_appl([{NodelogPod,"common",common}]),
    [{ok,_,_,_}]=lib_infra_service:create_appl([{NodelogPod,"sd",sd}]),
    [{ok,_,_,_}]=lib_infra_service:create_infra_appl({NodelogPod,"nodelog",nodelog}),
    
    

    

    %%-- 





%    io:format("InfraServicePods !!! ~p~n",[{InfraServicePods,?MODULE,?FUNCTION_NAME}]),
    
       
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
 %   timer:sleep(2000),
 %  init:stop(),
    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------


%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

setup(_ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    
    
    ok.
