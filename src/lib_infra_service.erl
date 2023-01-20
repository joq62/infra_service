%%%-------------------------------------------------------------------
%%% @author c50 <joq62@c50>
%%% @copyright (C) 2023, c50
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2023 by c50 <joq62@c50>
%%%-------------------------------------------------------------------
-module(lib_infra_service).


-define(LogDir,"log_dir").
-define(LogFileName,"file.logs").
-define(SleepInterval,20*1000).
%% API
-export([
	 ensure_right_cookie/1,
	 orchistrate/0,
	 orchistrate/1,
	 start_parents_pods/0,
	 start_infra_appls/0,
	 start_user_appls/0 
	]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
orchistrate()->
    orchistrate(?SleepInterval).
orchistrate(SleepInterval)->
    timer:sleep(SleepInterval),
    ResultStartParentPods=rpc:call(node(),?MODULE,start_parents_pods,[],15*1000),
    ResultStartInfraAppls=rpc:call(node(),?MODULE,start_infra_appls,[],15*1000),
    ResultStartUserAppls=rpc:call(node(),?MODULE,start_user_appls,[],15*1000), 
    rpc:cast(node(),infra_service,orchistrate_result,[ResultStartParentPods,
						      ResultStartInfraAppls,
						      ResultStartUserAppls]).
    
   

%%%===================================================================
%%% Internal functions
%%%===================================================================
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
start_parents_pods()->
    {ok,StoppedParents}=parent_server:stopped_nodes(),
    [parent_server:create_node(Parent)||Parent<-StoppedParents],
    {ok,ActiveParents}=parent_server:active_nodes(),
    []=[{net_adm:ping(Pod1),rpc:call(Pod1,net_adm,ping,[Pod2],5000)}||Pod1<-ActiveParents,
								   Pod2<-ActiveParents,
								   Pod1/=Pod2],						
    {ok,StoppedPods}=pod_server:stopped_nodes(),
    [create_pod(Pod)||Pod<-StoppedPods],
    [rpc:call(Pod1,net_adm,ping,[Pod2],5000)||Pod1<-ActiveParents,
					      Pod2<-StoppedPods,
					      Pod1/=Pod2],
    {ok,StoppedApplInfoLists}=appl_server:stopped_appls(),
    StoppedPod=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					   pod==App],
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedPod,[])],
    
  
    ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_infra_appls()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    {ok,StoppedApplInfoLists}=appl_server:stopped_appls(),    
    StoppedNodelog=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					    nodelog==App],
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedNodelog,[])],
    
    StoppedDbEtcd=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					   db_etcd==App],
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedDbEtcd,[])],

    StoppedInfraService=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					   infra_service==App],
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedInfraService,[])],
    {ok,ActiveApplsInfoList}=appl_server:active_appls(),
    true=lists:keymember(nodelog,3,ActiveApplsInfoList),
    true=lists:keymember(db_etcd,3,ActiveApplsInfoList),
    true=lists:keymember(infra_service,3,ActiveApplsInfoList),

    % config db_etcd
    [{DbEtcdNode,DbEtcdApp}]=[{Node,App}||{Node,_ApplSpec,App}<-ActiveApplsInfoList,
					  db_etcd==App],

    false=rpc:call(DbEtcdNode,DbEtcdApp,is_config,[],5000),
    ok=rpc:call(DbEtcdNode,DbEtcdApp,config,[],5000),
    true=rpc:call(DbEtcdNode,DbEtcdApp,is_config,[],5000),

    [{NodelogNode,_NodelogApp}]=[{Node,App}||{Node,_ApplSpec,App}<-ActiveApplsInfoList,
					    nodelog==App],
  
    false=rpc:call(NodelogNode,nodelog,is_config,[],5000),
    {ok,PodDir}=db_pod_desired_state:read(pod_dir,NodelogNode),
    PathLogDir=filename:join(PodDir,?LogDir),
    rpc:call(NodelogNode,file,del_dir_r,[PathLogDir],5000),
    ok=rpc:call(NodelogNode,file,make_dir,[PathLogDir],5000),
    PathLogFile=filename:join([PathLogDir,?LogFileName]),
    ok=rpc:call(NodelogNode,nodelog,config,[PathLogFile],5000),
    true=rpc:call(NodelogNode,nodelog,is_config,[],5000),
  
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_user_appls()->
    {ok,StoppedApplInfoLists}=appl_server:stopped_appls(),
    StoppedUserApplications=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
						     pod/=App,
						     db_etcd/=App,
						     nodelog/=App,
						     infra_service/=App],
    _=[{error,Reason}||{error,Reason}<-create_appl(StoppedUserApplications,[])],


    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ensure_right_cookie(ClusterSpec)->
    {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
    erlang:set_cookie(node(),list_to_atom(Cookie)),
    
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

create_pod(PodNode)->
    {ok,ParentNode}=db_pod_desired_state:read(parent_node,PodNode),
    {ok,NodeName}=db_pod_desired_state:read(node_name,PodNode),
    {ok,PodDir}=db_pod_desired_state:read(pod_dir,PodNode),
    {ok,PaArgsList}=db_pod_desired_state:read(pa_args_list,PodNode),
    {ok,EnvArgs}=db_pod_desired_state:read(env_args,PodNode),
    pod_server:create_pod(ParentNode,NodeName,PodDir,PaArgsList,EnvArgs).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_appl([],Acc)->
    Acc;
create_appl([{PodNode,ApplSpec,App}|T],Acc)->
    Result=appl_server:create_appl(ApplSpec,PodNode),
    io:format("Ping  ~p~n",[{rpc:call(PodNode,App,ping,[],2000),PodNode,ApplSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("Creat Appl Result ~p~n",[{Result,PodNode,ApplSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    create_appl(T,[{Result,PodNode,ApplSpec,App}|Acc]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
