%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_pod).
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([
	 create_node/5,
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
    Result=case sd:call(db_etcd,db_pod_desired_state,get_all_id,[],5000) of
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
    Result= case sd:call(db_etcd,db_pod_desired_state,get_all_id,[],5000) of
		  {error,Reason}->
		    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error: db_pod_desired_state,get_all_id: ",Reason,?MODULE,?LINE]]),
		    {error,Reason};
		AllNodes->
		    RunningNodesDir=[{Node,sd:call(db_etcd,db_pod_desired_state,read,[pod_dir,Node],5000)}||Node<-AllNodes,
													    pong==net_adm:ping(Node)],
		    ActiveNodes=[Node||{Node,{ok,PodDir}}<-RunningNodesDir,
				       rpc:call(Node,filelib,is_dir,[PodDir],5000)],
		    [rpc:call(Node,init,stop,[],3000)||{Node,{ok,_PodDir}}<-RunningNodesDir,
						       false==lists:member(Node,ActiveNodes)],
		    {ok,ActiveNodes}
	    end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stopped_nodes()->
    AllNodes=sd:call(db_etcd,db_pod_desired_state,get_all_id,[],5000),
    Result=case active_nodes() of
	       {ok,ActiveNodes}->		 
		   StoppedNodes=[Node||Node<-AllNodes,
				       false==lists:member(Node,ActiveNodes)],
		   {ok,StoppedNodes};
	       Reason->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error: active_nodes: ",Reason,?MODULE,?LINE]]),
		   {error,Reason}
	   end,
    Result.
    
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_node(PodNode)->
    Result=case sd:call(db_etcd,db_pod_desired_state,read,[parent_node,PodNode],5000) of
	{ok,ParentNode}->
		   case sd:call(db_etcd,db_pod_desired_state,read,[node_name,PodNode],5000) of
		       {ok,NodeName}->
			   case sd:call(db_etcd,db_pod_desired_state,read,[pod_dir,PodNode],5000) of
			       {ok,PodDir}->
				   case sd:call(db_etcd,db_pod_desired_state,read,[pa_args_list,PodNode],5000) of
				       {ok,PaArgsList}->
					   case sd:call(db_etcd,db_pod_desired_state,read,[env_args,PodNode],5000) of 
					       {ok,EnvArgs}->
						   create_node(ParentNode,NodeName,PodDir,PaArgsList,EnvArgs);
					       Reason->
						   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error: db_pod_desired_state,read,[env_args,PodNode ",Reason,PodNode,?MODULE,?LINE]]),
						   {error,Reason}
					   end;
				       Reason->
					   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error: db_pod_desired_state,read,[pa_args_list,PodNode ",Reason,PodNode,?MODULE,?LINE]]),
					   {error,Reason}
				   end;
			       Reason->
				   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error: db_pod_desired_state,read,[pod_dir,PodNode ",Reason,PodNode,?MODULE,?LINE]]),
				   {error,Reason}
			   end;
		       Reason->
			   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error: db_pod_desired_state,read,[node_name,PodNode ",Reason,PodNode,?MODULE,?LINE]]),
			   {error,Reason}
		   end;
	       Reason->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error: db_pod_desired_state,read,[parent_node,PodNode ",Reason,PodNode,?MODULE,?LINE]]),
		   {error,Reason}
	   end,
    Result.

create_node(ParentNode,NodeName,PodDir,PaArgsList,EnvArgs)->
    Result=case rpc:call(ParentNode,net,gethostname,[],5000) of
	       {badrpc,Reason}->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error creating PodName on ParentNode",NodeName,ParentNode,Reason]]),
		   {badrpc,["Error  :",Reason,ParentNode]};
	       {ok,HostName}->
		   case rpc:call(ParentNode,erlang,get_cookie,[],5000) of 
		       {badrpc,Reason}->
			   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error creating PodName on ParentNode",NodeName,ParentNode,Reason]]),
			   {badrpc,["Error  :",Reason,ParentNode]};	
		       CookieAtom->
			   Cookie=atom_to_list(CookieAtom),
			   Args=" -setcookie "++Cookie++" "++EnvArgs,
			   case rpc:call(ParentNode,slave,start,[HostName,NodeName,Args],5000) of
			       {badrpc,Reason}->
				   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error creating PodName on ParentNode",NodeName,ParentNode,Reason]]),
				   {badrpc,Reason};
			       {error,Reason}->
				   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error creating PodName on ParentNode",NodeName,ParentNode,Reason]]),
				   {error,Reason};
			       {ok,SlaveNode}->
				   case net_kernel:connect_node(SlaveNode) of
				       false->
					   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error failed_connect ",SlaveNode]]),
					   {error,[failed_connect,SlaveNode]};
				       ignored->
					   {error,[ignored,SlaveNode]};
				       true->
					   rpc:call(SlaveNode,file,del_dir_r,[PodDir],5000),			  			  
					   case rpc:call(SlaveNode,file,make_dir,[PodDir],5000) of
					       {badrpc,Reason}->
						   rpc:call(SlaveNode,init,stop,[],1000),
						   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error creating PodName on ParentNode",NodeName,ParentNode,Reason]]),
						   {badrpc,Reason};
					       {error,Reason}->
						   rpc:call(SlaveNode,init,stop,[],1000),
						   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error creating PodName on ParentNode",NodeName,ParentNode,Reason]]),
						   {error,Reason};
					       ok->  
						   []=[{error,Path}||Path<-[PodDir|PaArgsList],
								     true/=rpc:call(SlaveNode,code,add_patha,[Path],5000)],
						   ok
					   end
				   end
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
  %  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["DBG  : ",node(),?MODULE,?FUNCTION_NAME,?LINE]]),
    {ok,Pods}=sd:call(db_etcd,db_cluster_spec,read,[pods,ClusterSpec],5000),
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
    
    [ParentNode]=[ParentNode||ParentNode<-sd:call(db_etcd,db_parent_desired_state,get_all_id,[],5000),
			      {ok,HostSpec}==sd:call(db_etcd,db_parent_desired_state,read,[host_spec,ParentNode],5000),
			      {ok,ClusterSpec}==sd:call(db_etcd,db_parent_desired_state,read,[cluster_spec,ParentNode],5000)],

    {ok,RootDir}=sd:call(db_etcd,db_cluster_spec,read,[root_dir,ClusterSpec],5000),
%    {ok,HostName}=db_host_spec:read,[hostname,HostSpec),
    BaseNodeName=ClusterSpec++"_pod",
    Result=load_info(NumPods,RootDir,BaseNodeName,ClusterSpec,HostSpec,ParentNode,[]),
    load_desired_state(T,ClusterSpec,[Result|Acc]).

load_info(0,_RootDir,_BaseNodeName,_ClusterSpec,_HostSpec,_ParentNode,Acc)->
    Acc;
load_info(N,RootDir,BaseNodeName,ClusterSpec,HostSpec,ParentNode,Acc)->
    NodeName=integer_to_list(N)++"_"++BaseNodeName,
    {ok,HostName}=sd:call(db_etcd,db_host_spec,read,[hostname,HostSpec],5000),
    PodNode=list_to_atom(NodeName++"@"++HostName),
    PodDir=filename:join(RootDir,NodeName),
    PaArgsList=[],
    EnvArgs=" ",             
    AppSpecList=[],
    Result=case sd:call(db_etcd,db_pod_desired_state,create,[PodNode,NodeName,PodDir,ParentNode,AppSpecList,ClusterSpec,HostSpec,PaArgsList,EnvArgs],5000) of
	       {atomic,ok}->
		   ok;
	       Reason->
		   {error,Reason}
	   end,
    load_info(N-1,RootDir,BaseNodeName,ClusterSpec,HostSpec,ParentNode,[Result|Acc]).
