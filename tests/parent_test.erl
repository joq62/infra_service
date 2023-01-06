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
    ok=create_check_nodes_test(ClusterSpec),
%    ok=check_status_desired_state(ClusterSpec),
  %  ok=create_connect(ClusterSpec,StartHostSpec),
%    ok=init_test(ClusterSpec,StartHostSpec),
        
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  %  init:stop(),
  %  timer:sleep(2000),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

create_check_nodes_test(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    %% Just for Testing  ensure nodes are stopped
    [rpc:call(Node,init,stop,[],3000)||Node<-db_parent_desired_state:get_all_id()],
    timer:sleep(2000),
    %% Check avaialble nodes before intitateing them 
    {ok,DesiredNodes}=parent_server:desired_nodes(),
    [Parent1,Parent2]=lists:sort(DesiredNodes),
    [Parent1,Parent2]=['c200_c201_parent@c200','c200_c201_parent@c201'],
    {ok,[]}=parent_server:active_nodes(),
    {ok,StoppedNodes}=parent_server:stopped_nodes(),
    [Parent1,Parent2]=lists:sort(StoppedNodes),

    % Create First
    ok=create_parent(Parent1),
    {ok,[Parent1]}=parent_server:active_nodes(),
    {ok,[Parent2]}=parent_server:stopped_nodes(),

     % Create second
    ok=create_parent(Parent2),
    {ok,Active2}=parent_server:active_nodes(),
    [Parent1,Parent2]=lists:sort(Active2),
    {ok,[]}=parent_server:stopped_nodes(),

    % Delete dir on Parent1
    {ok,RootDir}=db_cluster_spec:read(root_dir,ClusterSpec),
    ok=rpc:call(Parent1,file,del_dir_r,[RootDir],5000),
    false=rpc:call(Parent1,filelib,is_dir,[RootDir],5000),
    {ok,[Parent2]}=parent_server:active_nodes(),
    {ok,[Parent1]}=parent_server:stopped_nodes(),

    % stop Parent2
    rpc:call(Parent2,init,stop,[],5000),
    timer:sleep(2000), 
    {ok,[]}=parent_server:active_nodes(),
    {ok,StoppedNodes2}=parent_server:stopped_nodes(),
    [Parent1,Parent2]=lists:sort(StoppedNodes2),
    
    ok.

create_parent(ParentNode)->
    % Start a detached vm as ParentNode
    ok=lib_parent:create_node(ParentNode),
    pong=net_adm:ping(ParentNode),
    
    {ok,ClusterSpec}=db_parent_desired_state:read(cluster_spec,ParentNode),
    {ok,RootDir}=db_cluster_spec:read(root_dir,ClusterSpec),
    % Delete and Creat cluster dir 
    rpc:call(ParentNode,file,del_dir_r,[RootDir],5000),
    ok=rpc:call(ParentNode,file,make_dir,[RootDir],5000),
       
    % Add the right paths 

    % Load and start common 
    % Load and start db_etcd
    % Load and start resource_ciscovery
    
    ok.
        

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
load_desired_state_test(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
   
    % Load the database db_etcd
    ok=parent_server:load_desired_state(ClusterSpec),
   
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
 %   {ok,_}=pod_server:start(),
 %   pong=pod_server:ping(),
 %   {ok,_}=appl_server:start(),
 %   pong=appl_server:ping(),



    
    [rd:add_local_resource(Type,node())||Type<-?LocalTypes],
    [rd:add_target_resource_type(Type)||Type<-?TargetTypes],
    ok=rd:trade_resources(),

    {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
    true=erlang:set_cookie(node(),list_to_atom(Cookie)),

      
    ok.
