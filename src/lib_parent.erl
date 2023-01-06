%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_parent).
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([
	 create_node/1,
	 load_desired_state/1,
	 desired_nodes/0,
	 active_nodes/0,
	 stopped_nodes/0

	]).


%% ====================================================================
%% External functions
%% ====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
desired_nodes()->
    Result=case db_parent_desired_state:get_all_id() of
	       {error,Reason}->
		   {error,Reason};
	       []->
		   {error,["No desired parent nodes are declared: "]};
	       Nodes->
		   {ok,Nodes}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
active_nodes()->
    AllParentNodes=db_parent_desired_state:get_all_id(),
    [Parent1|_]=AllParentNodes,
    {ok,ClusterSpec}=db_parent_desired_state:read(cluster_spec,Parent1),
    {ok,RootDir}=db_cluster_spec:read(root_dir,ClusterSpec),
    RunningNodes=[Node||Node<-AllParentNodes,
			pong==net_adm:ping(Node)],
    ActiveNodes=[Node||Node<-RunningNodes,
		       rpc:call(Node,filelib,is_dir,[RootDir],5000)],
    [rpc:call(Node,init,stop,[],3000)||Node<-RunningNodes,
				       false==lists:member(Node,ActiveNodes)],
    {ok,ActiveNodes}.
    %%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stopped_nodes()->
    AllParentNodes=db_parent_desired_state:get_all_id(),
    {ok,ActiveNodes}=active_nodes(),
    StoppedNodes=[Node||Node<-AllParentNodes,
			false==lists:member(Node,ActiveNodes)],
    {ok,StoppedNodes}.
    
    
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_node(ParentNode)->
    {ok,HostSpec}=db_parent_desired_state:read(host_spec,ParentNode),
    {ok,NodeName}=db_parent_desired_state:read(node_name,ParentNode),
    {ok,ClusterSpec}=db_parent_desired_state:read(cluster_spec,ParentNode),
    {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
    EnvArgs=" -detached ",
    PaArgs=" ",
    TimeOut=10*1000,
    Result=case ops_ssh:create(HostSpec,NodeName,Cookie,PaArgs,EnvArgs,TimeOut) of
	       {error,Reason}->
		   {error,Reason};
	       {ok,ParentNode}->
		   ok
	   end,
    Result.    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

load_desired_state(ClusterSpec)->
    {ok,Pods}=db_cluster_spec:read(pods,ClusterSpec),
    LoadResult=[{error,Reason}|| {error,Reason}<-load_desired_state(Pods,ClusterSpec,[])],
    case LoadResult of
	[]->
	    ok;
	ErrorList ->
	    {error,ErrorList}
    end.
    
load_desired_state([],_ClusterSpec,Acc)->
    Acc;
load_desired_state([{_NumPods,HostSpec}|T],ClusterSpec,Acc) ->
    false=lists:member({ok,HostSpec},Acc),
    {ok,RootDir}=db_cluster_spec:read(root_dir,ClusterSpec),
    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
    NodeName=ClusterSpec++"_parent",
    ParentNode=list_to_atom(NodeName++"@"++HostName),
    RootPaArgs=" -pa "++RootDir++" ",
    PathCommonFuns=filename:join([RootDir,"*","ebin"]),
    CommonFunsPaArgs=" -pa "++PathCommonFuns,
    EnvArgs=" ",
    Result=case db_parent_desired_state:create(ParentNode,NodeName,ClusterSpec,HostSpec,
					       RootPaArgs,CommonFunsPaArgs,EnvArgs) of
	       {atomic,ok}->
		   ok;
	       Reason->
		   {error,Reason}
	   end,
    load_desired_state(T,ClusterSpec,[Result|Acc]).
