%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_console). 
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([
	 load_start_appl/3
	 
	 

	]).

-export([
	 create_node/5,
	 load_desired_state/1,
	 desired_nodes/0,
	 active_nodes/0,
	 stopped_nodes/0

	]).


%% ====================================================================
%% External functions
%% ====================================================================
install_parents(ClusterSpec)->ok.
install_pods(ClusterSpec)->ok.
install_appls(ClusterSpec)->ok.

get_started_appls(ClusterSpec)->ok.
get_stopped_appls(ClusterSpec)->ok.
which_appls(PodNode)->ok .
where_is_appl(ClusterSpec)->ok.
load_start_appl(Appl,PodNode,ClusterSpec)->ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
desired_nodes()->
    Result=case db_pod_desired_state:get_all_id() of
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
    AllNodes=db_pod_desired_state:get_all_id(),
    RunningNodesDir=[{Node,db_pod_desired_state:read(pod_dir)}||Node<-AllNodes,
								pong==net_adm:ping(Node)],
    ActiveNodes=[Node||{Node,{ok,PodDir}}<-RunningNodesDir,
		       rpc:call(Node,filelib,is_dir,[PodDir],5000)],
    [rpc:call(Node,init,stop,[],3000)||{Node,{ok,_PodDir}}<-RunningNodesDir,
				       false==lists:member(Node,ActiveNodes)],
    {ok,ActiveNodes}.
    %%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stopped_nodes()->
    AllNodes=db_pod_desired_state:get_all_id(),
    {ok,ActiveNodes}=active_nodes(),
    StoppedNodes=[Node||Node<-AllNodes,
			false==lists:member(Node,ActiveNodes)],
    {ok,StoppedNodes}.
    
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_node(ParentNode,NodeName,PodDir,PaArgsList,EnvArgs)->
    {ok,HostName}=rpc:call(ParentNode,net,gethostname,[],5000),
    Cookie=atom_to_list(rpc:call(ParentNode,erlang,get_cookie,[],5000)),
    Args=" -setcookie "++Cookie++" "++EnvArgs,
    Result=case rpc:call(ParentNode,slave,start,[HostName,NodeName,Args],5000) of
	       {badrpc,Reason}->
		   rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error creating PodName on ParentNode",NodeName,ParentNode,Reason]]),
		   {badrpc,Reason};
	       {error,Reason}->
		   rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error creating PodName on ParentNode",NodeName,ParentNode,Reason]]),
		   {error,Reason};
	       {ok,SlaveNode}->
		   case net_kernel:connect_node(SlaveNode) of
		       false->
			   rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error failed_connect ",SlaveNode]]),
			   {error,[failed_connect,SlaveNode]};
		       ignored->
			   {error,[ignored,SlaveNode]};
		       true->
			   rpc:call(SlaveNode,file,del_dir_r,[PodDir],5000),			  			  
			   case rpc:call(SlaveNode,file,make_dir,[PodDir],5000) of
			       {badrpc,Reason}->
				   rpc:call(SlaveNode,init,stop,[],1000),
				   rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error creating PodName on ParentNode",NodeName,ParentNode,Reason]]),
				   {badrpc,Reason};
			       {error,Reason}->
				   rpc:call(SlaveNode,init,stop,[],1000),
				   rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error creating PodName on ParentNode",NodeName,ParentNode,Reason]]),
				   {error,Reason};
			       ok->  
				   []=[{error,Path}||Path<-[PodDir|PaArgsList],
						     true/=rpc:call(SlaveNode,code,add_patha,[Path],5000)],
				   rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Pod node succesfully created with Dir  ",SlaveNode,PodDir]]),
				   ok
			   end
		   end
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
load_desired_state([{NumPods,HostSpec}|T],ClusterSpec,Acc) ->
    false=lists:member({ok,HostSpec},Acc),
    
    [ParentNode]=[ParentNode||ParentNode<-db_parent_desired_state:get_all_id(),
			      {ok,HostSpec}==db_parent_desired_state:read(host_spec,ParentNode),
			      {ok,ClusterSpec}==db_parent_desired_state:read(cluster_spec,ParentNode)],

    {ok,RootDir}=db_cluster_spec:read(root_dir,ClusterSpec),
%    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
    BaseNodeName=ClusterSpec++"_pod",
    Result=load_info(NumPods,RootDir,BaseNodeName,ClusterSpec,HostSpec,ParentNode,[]),
    load_desired_state(T,ClusterSpec,[Result|Acc]).

load_info(0,_RootDir,_BaseNodeName,_ClusterSpec,_HostSpec,_ParentNode,Acc)->
    Acc;
load_info(N,RootDir,BaseNodeName,ClusterSpec,HostSpec,ParentNode,Acc)->
    NodeName=integer_to_list(N)++"_"++BaseNodeName,
    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
    PodNode=list_to_atom(NodeName++"@"++HostName),
    PodDir=filename:join(RootDir,NodeName),
    PaArgsList=[],
    EnvArgs=" ",             
    AppSpecList=[],
    Result=case db_pod_desired_state:create(PodNode,NodeName,PodDir,ParentNode,AppSpecList,ClusterSpec,HostSpec,PaArgsList,EnvArgs) of
	       {atomic,ok}->
		   ok;
	       Reason->
		   {error,Reason}
	   end,
    load_info(N-1,RootDir,BaseNodeName,ClusterSpec,HostSpec,ParentNode,[Result|Acc]).
