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
    Result=case sd:call(db_etcd,db_parent_desired_state,get_all_id,[],5000) of
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
    AllNodes=sd:call(db_etcd,db_parent_desired_state,get_all_id,[],5000),
    [Node1|_]=AllNodes,
    {ok,ClusterSpec}=sd:call(db_etcd,db_parent_desired_state,read,[cluster_spec,Node1],5000),
    {ok,RootDir}=sd:call(db_etcd,db_cluster_spec,read,[root_dir,ClusterSpec],5000),
    RunningNodes=[Node||Node<-AllNodes,
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
    AllNodes=sd:call(db_etcd,db_parent_desired_state,get_all_id,[],5000),
    {ok,ActiveNodes}=active_nodes(),
    StoppedNodes=[Node||Node<-AllNodes,
			false==lists:member(Node,ActiveNodes)],
    {ok,StoppedNodes}.
    
    
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_node(ParentNode)->
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: create_node ParentNode : ",ParentNode,?MODULE,?LINE]]),
    {ok,HostSpec}=sd:call(db_etcd,db_parent_desired_state,read,[host_spec,ParentNode],5000),

    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: create_node HostSpec : ",HostSpec,?MODULE,?LINE]]),

    {ok,NodeName}=sd:call(db_etcd,db_parent_desired_state,read,[node_name,ParentNode],5000),

    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: create_node NodeName : ",NodeName,?MODULE,?LINE]]),
    
    {ok,ClusterSpec}=sd:call(db_etcd,db_parent_desired_state,read,[cluster_spec,ParentNode],5000),

    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: create_node ClusterSpec : ",ClusterSpec,?MODULE,?LINE]]),

    {ok,Cookie}=sd:call(db_etcd,db_cluster_spec,read,[cookie,ClusterSpec],5000),

    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: create_node Cookie : ",Cookie,?MODULE,?LINE]]),

    EnvArgs=" -detached ",
    PaArgs=" ",
    TimeOut=10*1000,
    Result=case rpc:call(node(),ops_ssh,creat,[HostSpec,NodeName,Cookie,PaArgs,EnvArgs,TimeOut],TimeOut+1000) of
	       {error,Reason}->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error: ops_ssh:create : ",Reason,?MODULE,?LINE]]),
		   {error,Reason};
	       {ok,ParentNode}->
		   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: OK create_node  : ",ParentNode,?MODULE,?LINE]]),
		   ok;
	       Error ->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error: ops_ssh:create : ",Error,?MODULE,?LINE]]),
		   {error,Error}
	   end,
    Result.    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

load_desired_state(ClusterSpec)->
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
load_desired_state([{_NumPods,HostSpec}|T],ClusterSpec,Acc) ->
    false=lists:member({ok,HostSpec},Acc),
    {ok,RootDir}=sd:call(db_etcd,db_cluster_spec,read,[root_dir,ClusterSpec],5000),
    {ok,HostName}=sd:call(db_etcd,db_host_spec,read,[hostname,HostSpec],5000),
    NodeName=ClusterSpec++"_parent",
    ParentNode=list_to_atom(NodeName++"@"++HostName),
    RootPaArgs=" -pa "++RootDir++" ",
    PathCommonFuns=filename:join([RootDir,"*","ebin"]),
    CommonFunsPaArgs=" -pa "++PathCommonFuns,
    EnvArgs=" ",
    Result=case sd:call(db_etcd,db_parent_desired_state,create,[ParentNode,NodeName,ClusterSpec,HostSpec,
					       RootPaArgs,CommonFunsPaArgs,EnvArgs],5000) of
	       {atomic,ok}->
		   ok;
	       Reason->
		   {error,Reason}
	   end,
    load_desired_state(T,ClusterSpec,[Result|Acc]).
