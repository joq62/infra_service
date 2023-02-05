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
    {ok,ActiveParents}=infra_service_test:start_parents(),
    io:format("ActiveParents !!! ~p~n",[{ActiveParents,?MODULE,?FUNCTION_NAME}]),

    NodelogPods=lib_infra_service:create_pods_based_appl("nodelog"),
    io:format("NodelogPods !!! ~p~n",[{NodelogPods,?MODULE,?FUNCTION_NAME}]),

    DbEtcdPods=lib_infra_service:create_pods_based_appl("db_etcd"),
    io:format("DbEtcdPods !!! ~p~n",[{DbEtcdPods,?MODULE,?FUNCTION_NAME}]),

    InfraServicePods=lib_infra_service:create_pods_based_appl("infra_service"),
    io:format("InfraServicePods !!! ~p~n",[{InfraServicePods,?MODULE,?FUNCTION_NAME}]),
    
       
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
