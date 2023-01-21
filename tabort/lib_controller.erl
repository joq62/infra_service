%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_controller).
  

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports

-export([
	 load_desired_state/1,
	 orchistrate/0 

	]).


%% ====================================================================
%% External functions
%% ====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load_desired_state(ClusterSpec)->
    ok=parent_server:load_desired_state(ClusterSpec),
    ok=pod_server:load_desired_state(ClusterSpec),
    ok=appl_server:load_desired_state(ClusterSpec),
    
    ok.
    




%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
orchistrate()->
    ResultParent=orchistrate_parents(),
    if
	ResultParent/=ok->
	    rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error creating parent nodes ",ResultParent]]);
	true->
	    ok
    end,
    ResultPods=orchistrate_pods(),
    if
	ResultPods/=ok->
	    rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error creating pod nodes ",ResultPods]]);
	true->
	    ok
    end,
    ResultAppls=orchistrate_appls(),
    if
	ResultAppls/=ok->
	    rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error creating applications on Nodes ",ResultAppls]]);
	true->
	    ok
    end,
    ok.



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
orchistrate_parents()->
   Result=case check_parents_to_start() of
	      []->
		  ok;
	      ParentsToStart->
		  CreateResult=[parent_server:create_node(ParentNode)||ParentNode<-ParentsToStart],
		  ErrorList=[{error,Reason}||{error,Reason}<-CreateResult],  
		  case ErrorList of
		      []->
			  ok;
		      ErrorList ->
			  {error,ErrorList}
		  end
	  end,
    Result.
 
check_parents_to_start()->
    DesiredParentNodes=parent_server:desired_nodes(),
    ActiveParentNodes=parent_server:active_nodes(),
    ParentsToStart=[ParentNode||ParentNode<-DesiredParentNodes,
				true/=lists:member(ParentNode,ActiveParentNodes)],
    ParentsToStart.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
orchistrate_pods()->
   Result=case check_pods_to_start() of
	      []->
		  ok;
	      PodsToStart->
		  CreateResult=[pods_server:create_node(PodNode)||PodNode<-PodsToStart],
		  ErrorList=[{error,Reason}||{error,Reason}<-CreateResult],  
		  case ErrorList of
		      []->
			  ok;
		      ErrorList ->
			  {error,ErrorList}
		  end
	  end,
    Result.
 
check_pods_to_start()->
    DesiredPodNodes=pod_server:desired_nodes(),
    ActivePodNodes=pod_server:active_nodes(),
    PoddsToStart=[PodNode||PodNode<-DesiredPodNodes,
				true/=lists:member(PodNode,ActivePodNodes)],
    PoddsToStart.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
orchistrate_appls()->
   Result=case check_appls_to_start() of
	      []->
		  ok;
	      ApplsToStart->
		  CreateResult=[appls_server:create_node(Appl,PodNode)||{Appl,PodNode}<-ApplsToStart],
		  ErrorList=[{error,Reason}||{error,Reason}<-CreateResult],  
		  case ErrorList of
		      []->
			  ok;
		      ErrorList ->
			  {error,ErrorList}
		  end
	  end,
    Result.
 
check_appls_to_start()->
    DesiredAppls=appls_server:desired_appls(),
    ActiveAppls=appls_server:active_appls(),
    ApplsToStart=[{Appl,PodNode}||{Appl,PodNode}<-DesiredAppls,
				  true/=lists:member({Appl,PodNode},ActiveAppls)],
    ApplsToStart.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
missing_connect_nodes(ClusterSpec)->
    [Node||Node<-db_cluster_instance:nodes(connect,ClusterSpec), 
	   pang=:=net_adm:ping(Node)].
present_connect_nodes(ClusterSpec)->
    [Node||Node<-db_cluster_instance:nodes(connect,ClusterSpec), 
	   pong=:=net_adm:ping(Node)].
wanted_state(ClusterSpec)->
    Connect=[{Node,db_cluster_instance:read(pod_dir,ClusterSpec,Node)}||Node<-present_connect_nodes(ClusterSpec)],
    ConnectNoDirs=[Node||{Node,{ok,PodDir}}<-Connect,
			    true/=rpc:call(Node,filelib,is_dir,[PodDir],2000)],
    [rpc:call(Node,init,stop,[],2000)||Node<-ConnectNoDirs],
    MissingConnectNodes=lists:append(ConnectNoDirs,missing_connect_nodes(ClusterSpec)),  
    NodesToConnect=db_cluster_instance:nodes(connect,ClusterSpec),
    [glurk:create_connect_node(ClusterSpec,PodNode,NodesToConnect)||PodNode<-MissingConnectNodes],
    ok.
