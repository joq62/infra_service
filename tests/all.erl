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
	 notice/0,warning/0,alert/0,
	 stop/0,stop/1,
	 all_apps/0,
	 
	 print/2]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
stop()->
    stop(c200_c201_parent@c201).

stop(N)->
    rpc:call(N,init,stop,[],5000).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


all_apps()->
    [InfraPod|_]=sd:get_node(infra_service),
    running_apps(InfraPod).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
notice()->
    sd:call(nodelog,nodelog,read,[notice],2000).
warning()->
    sd:call(nodelog,nodelog,read,[warning],2000).
alert()->
    sd:call(nodelog,nodelog,read,[alert],2000).
    
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
    %%-- create pods and load appl
       
    %%-- create nodelog appl
   % glurk=rpc:call(NodelogPod,application,which_applications,[],5000),
    [{ok,NodelogPod,NodelogApplSpec}]=lib_infra_service:create_pods_based_appl("nodelog"),
    [{ok,_,_,_}]=lib_infra_service:create_appl([{NodelogPod,"common",common}]),
    [{ok,_,_,_}]=lib_infra_service:create_appl([{NodelogPod,"sd",sd}]),
    ok=lib_infra_service:create_infra_appl({NodelogPod,"nodelog",nodelog},ClusterSpec),
    io:format("Phase 1 Running nodes !!! ~p~n",[{running_nodes(NodelogPod),?MODULE,?FUNCTION_NAME}]),
    io:format("Phase 1 Running apps !!! ~p~n",[{running_apps(NodelogPod),?MODULE,?FUNCTION_NAME}]),

    %%-- create db_etcd
    [{ok,DbPod,DbApplSpec}]=lib_infra_service:create_pods_based_appl("db_etcd"),
    [{ok,_,_,_}]=lib_infra_service:create_appl([{DbPod,"common",common}]),
    [{ok,_,_,_}]=lib_infra_service:create_appl([{DbPod,"sd",sd}]),
    ok=lib_infra_service:create_infra_appl({DbPod,"db_etcd",db_etcd},ClusterSpec),
    application:stop(db_etcd),
    [DbPod]=sd:get_node(db_etcd),
    io:format("DbPod ~p~n",[{DbPod,?MODULE,?FUNCTION_NAME,?LINE}]),
    %%- Initiate db_etcd with desired_State !!
    
    ok=parent_server:load_desired_state(ClusterSpec),
    ok=pod_server:load_desired_state(ClusterSpec),
    ok=appl_server:load_desired_state(ClusterSpec),

    
    %%-- create infra_service
    [{ok,InfraPod,InfraApplSpec}]=lib_infra_service:create_pods_based_appl("infra_service"),
    [{ok,_,_,_}]=lib_infra_service:create_appl([{InfraPod,"common",common}]),
    [{ok,_,_,_}]=lib_infra_service:create_appl([{InfraPod,"sd",sd}]),
    ok=lib_infra_service:create_infra_appl({InfraPod,"infra_service",infra_service},ClusterSpec),
    true=rpc:cast(InfraPod,infra_service,start_orchistrate,[]),
    

    
    io:format("Phase 3 Running nodes !!! ~p~n",[{running_nodes(NodelogPod),?MODULE,?FUNCTION_NAME}]),
    io:format("Phase 3 Running apps !!! ~p~n",[{running_apps(NodelogPod),?MODULE,?FUNCTION_NAME}]),

    WhichApplications2=[{Node,rpc:call(Node,application,which_applications,[],5000)}||Node<-nodes()],
    io:format("WhichApplications2 !!! ~p~n",[{WhichApplications2,?MODULE,?FUNCTION_NAME,?LINE}]),
    
       
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
 %   timer:sleep(2000),
 %  init:stop(),
    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% -------------------------------------------------------------------
running_apps(Node)->
    Nodes=lists:delete(node(),[Node|rpc:call(Node,erlang,nodes,[],5000)]),
    [{N,rpc:call(N,application,which_applications,[],5000)}||N<-Nodes].

running_nodes(Node)->
    
    lists:delete(node(),[Node|rpc:call(Node,erlang,nodes,[],5000)]).

%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

setup(_ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    
    
    ok.
