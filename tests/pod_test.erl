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
-module(pod_test).      
 
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
    ok=create_parents_test(ClusterSpec),
    ok=create_check_nodes_test(ClusterSpec),

  
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

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

create_check_nodes_test(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {ok,DesiredNodes}=pod_server:desired_nodes(), 
    SortedDesiredNodes=lists:sort(DesiredNodes),
    [
     '1_c200_c201_pod@c200','1_c200_c201_pod@c201',
     '2_c200_c201_pod@c200','2_c200_c201_pod@c201',
     '3_c200_c201_pod@c200','3_c200_c201_pod@c201',
     '4_c200_c201_pod@c200','4_c200_c201_pod@c201',
     '5_c200_c201_pod@c200','5_c200_c201_pod@c201',
     '6_c200_c201_pod@c200','6_c200_c201_pod@c201'
    ]=SortedDesiredNodes,
   
    {ok,[]}=pod_server:active_nodes(),
    {ok,StoppedNodes}=pod_server:stopped_nodes(),
    SortedStoppedNodes=lists:sort(StoppedNodes),
    SortedDesiredNodes=SortedStoppedNodes,

    %% Start the PodNodes
    []=[PodNode||PodNode<-SortedStoppedNodes,
		 ok/=create_pod(PodNode)],
    
    {ok,AllParentNodes}=parent_server:desired_nodes(),
    Nodes=[{Node,rpc:call(Node,erlang,nodes,[],2000)}||Node<-AllParentNodes],
    io:format("Nodes ~p~n",[{Nodes,?MODULE,?FUNCTION_NAME}]),
    
    
    ok.


create_pod(PodNode)->
    {PodNode,NodeName,PodDir,ParentNode,PaArgList,_ApplSpecList,_HostSpec,EnvArgs}=db_pod_desired_state:read(PodNode),
    pod_server:create_pod(ParentNode,NodeName,PodDir,PaArgList,EnvArgs).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_parents_test(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    %% Just for Testing  ensure nodes are stopped
     [rpc:call(Node,init,stop,[],3000)||Node<-db_parent_desired_state:get_all_id()],
    timer:sleep(2000),
    %% Check avaialble nodes before intitateing them 
    {ok,DesiredParentNodes}=parent_server:desired_nodes(),
    []=[{error,Parent}||Parent<-DesiredParentNodes,
		     ok/=create_parent(Parent)],
    {ok,[]}=parent_server:stopped_nodes(),
    PongParents=[{Node1,Node2,rpc:call(Node1,net_adm,ping,[Node2],5000)}||Node1<-DesiredParentNodes,
									  Node2<-DesiredParentNodes,
									  Node1/=Node2],
    io:format("PongParents ~p~n",[{PongParents,?MODULE,?FUNCTION_NAME}]),
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
    ok=pod_server:load_desired_state(ClusterSpec),
   
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
 %   {ok,_}=appl_server:start(),
 %   pong=appl_server:ping(),



    
    [rd:add_local_resource(Type,node())||Type<-?LocalTypes],
    [rd:add_target_resource_type(Type)||Type<-?TargetTypes],
    ok=rd:trade_resources(),

    {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
    true=erlang:set_cookie(node(),list_to_atom(Cookie)),

      
    ok.
