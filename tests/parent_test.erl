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
-module(parent_test).      
 
-export([start/2]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start(ClusterSpec,_StartHostSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(ClusterSpec),
    ok=load_desired_state_test(ClusterSpec),
  % ok=create_connect(ClusterSpec,StartHostSpec),
%    ok=init_test(ClusterSpec,StartHostSpec),
        
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  %  init:stop(),
  %  timer:sleep(2000),
    ok.




%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
load_desired_state_test(ClusterSpec)->
    ok=parent_server:load_desired_state(ClusterSpec),
    glurk=db_parent_desired_state:read_all(),
    
    

    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
create_connect(ClusterSpec,StartHostSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    %% Ensure that config is started proprely , strange!!
    ok=db_config:create_table(),
    {atomic,ok}=db_config:set(cluster_spec,ClusterSpec),
    ClusterSpec=db_config:get(cluster_spec),
    
      % Create first connect node with right cookie
    {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
    erlang:set_cookie(node(),list_to_atom(Cookie)),
    
    % create connect nodes
    {ok,_}=connect_server:create_dbase_info(ClusterSpec), 
    ConnectStart=connect_server:create_connect_nodes(ClusterSpec),
    []=[{error,Reason}||{error,Reason}<-ConnectStart],
    
    % Create connect pod
    
    % Create controller and  worker nodes 

    {Controllers,[]}=pod_server:create_controller_pods(ClusterSpec),
    {Workers,[]}=pod_server:create_worker_pods(ClusterSpec),
   
    % Install basic applications on each node but connect 
    RC1=[appl_server:new_on_pod("common",PodNode,ClusterSpec,10*1000)||PodNode<-Controllers],
    []=[{error,Reason}||{error,Reason}<-RC1],
    []=[{error,PodNode}||PodNode<-Controllers,
			 false==lists:keymember(common,1,rpc:call(PodNode,application,which_applications,[],1000))],

    RC2=[appl_server:new_on_pod("resource_discovery",PodNode,ClusterSpec,10*1000)||PodNode<-Controllers],
    []=[{error,Reason}||{error,Reason}<-RC2],
    []=[{error,PodNode}||PodNode<-Controllers,
			 false==lists:keymember(resource_discovery,1,rpc:call(PodNode,application,which_applications,[],1000))],

    RW1=[appl_server:new_on_pod("common",PodNode,ClusterSpec,10*1000)||PodNode<-Workers],
    []=[{error,Reason}||{error,Reason}<-RW1],
    []=[{error,PodNode}||PodNode<-Workers,
			 false==lists:keymember(common,1,rpc:call(PodNode,application,which_applications,[],1000))],

    RW2=[appl_server:new_on_pod("resource_discovery",PodNode,ClusterSpec,10*1000)||PodNode<-Workers],
    []=[{error,Reason}||{error,Reason}<-RW2],
    []=[{error,PodNode}||PodNode<-Workers,
			 false==lists:keymember(resource_discovery,1,rpc:call(PodNode,application,which_applications,[],1000))],

    
	
    %% Initiate the first controller before hand over to infra_service
    [FirstController|_]=lists:sort(Controllers),
    
    ok=appl_server:new_on_pod("nodelog",FirstController,ClusterSpec,10*1000), 
    ok=appl_server:new_on_pod("db_etcd",FirstController,ClusterSpec,10*1000), 
    
  
    % hang up

    
    AllApps=[{PodNode,rpc:call(PodNode,application,which_applications,[],1000)}||PodNode<-lists:append(Controllers,Workers)],
    io:format("AllApps ~p~n",[{AllApps,?MODULE,?FUNCTION_NAME}]),

    

    % Kill install node
    

    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
init_test(ClusterSpec,StartHostSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    %% Ensure that config is started proprely , strange!!
    ok=db_config:create_table(),
    {atomic,ok}=db_config:set(cluster_spec,ClusterSpec),
    ClusterSpec=db_config:get(cluster_spec),
    
      % Create first connect node with right cookie
    {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
    erlang:set_cookie(node(),list_to_atom(Cookie)),
    
    % create connect nodes
    {ok,_}=connect_server:create_dbase_info(ClusterSpec), 
    ConnectStart=connect_server:create_connect_nodes(ClusterSpec),
    []=[{error,Reason}||{error,Reason}<-ConnectStart],
    
    % Create connect pod
    
    % Create controller and  worker nodes 

    {Controllers,[]}=pod_server:create_controller_pods(ClusterSpec),
    {Workers,[]}=pod_server:create_worker_pods(ClusterSpec),
   
    % Install basic applications on each node but connect 
    RC1=[appl_server:new_on_pod("common",PodNode,ClusterSpec,10*1000)||PodNode<-Controllers],
    []=[{error,Reason}||{error,Reason}<-RC1],
    []=[{error,PodNode}||PodNode<-Controllers,
			 false==lists:keymember(common,1,rpc:call(PodNode,application,which_applications,[],1000))],

    RC2=[appl_server:new_on_pod("resource_discovery",PodNode,ClusterSpec,10*1000)||PodNode<-Controllers],
    []=[{error,Reason}||{error,Reason}<-RC2],
    []=[{error,PodNode}||PodNode<-Controllers,
			 false==lists:keymember(resource_discovery,1,rpc:call(PodNode,application,which_applications,[],1000))],

    RW1=[appl_server:new_on_pod("common",PodNode,ClusterSpec,10*1000)||PodNode<-Workers],
    []=[{error,Reason}||{error,Reason}<-RW1],
    []=[{error,PodNode}||PodNode<-Workers,
			 false==lists:keymember(common,1,rpc:call(PodNode,application,which_applications,[],1000))],

    RW2=[appl_server:new_on_pod("resource_discovery",PodNode,ClusterSpec,10*1000)||PodNode<-Workers],
    []=[{error,Reason}||{error,Reason}<-RW2],
    []=[{error,PodNode}||PodNode<-Workers,
			 false==lists:keymember(resource_discovery,1,rpc:call(PodNode,application,which_applications,[],1000))],

    
	
    %% Initiate the first controller before hand over to infra_service
    [FirstController|_]=lists:sort(Controllers),
    
    ok=appl_server:new_on_pod("nodelog",FirstController,ClusterSpec,10*1000), 
    ok=appl_server:new_on_pod("db_etcd",FirstController,ClusterSpec,10*1000), 
    
  
    % hang up

    
    AllApps=[{PodNode,rpc:call(PodNode,application,which_applications,[],1000)}||PodNode<-lists:append(Controllers,Workers)],
    io:format("AllApps ~p~n",[{AllApps,?MODULE,?FUNCTION_NAME}]),

    

    % Kill install node
    

    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
-define(LocalTypes,[oam,nodelog,db_etcd]).
-define(TargetTypes,[oam,nodelog,db_etcd]).

setup(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=application:start(common),
    pong=common:ping(),
    ok=application:start(resource_discovery),
    pong=rd:ping(),
    ok=application:start(nodelog),
    pong=nodelog:ping(),
    ok=application:start(db_etcd),
    pong=db_etcd:ping(),
    
    ok=db_etcd:install(),

    {ok,_}=parent_server:start(),
    pong=parent_server:ping(),
    {ok,_}=pod_server:start(),
    pong=pod_server:ping(),
    {ok,_}=appl_server:start(),
    pong=appl_server:ping(),



    
    [rd:add_local_resource(Type,node())||Type<-?LocalTypes],
    [rd:add_target_resource_type(Type)||Type<-?TargetTypes],
    ok=rd:trade_resources(),
      
    ok.
