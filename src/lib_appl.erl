%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_appl).
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([
	 create_appl/2,
	 load_desired_state/1,
	 desired_appls/0,
	 active_appls/0,
	 stopped_appls/0

	]).


%% ====================================================================
%% External functions
%% ====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
desired_appls()->
    
    Result=case db_appl_desired_state:get_all_id() of
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
active_appls()->
    AllNodes=db_appl_desired_state:get_all_id(),
    RunningNodesDir=[{Node,db_appl_desired_state:read(pod_dir)}||Node<-AllNodes,
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
stopped_appls()->
    AllNodes=db_appl_desired_state:get_all_id(),
    {ok,ActiveNodes}=active_appls(),
    StoppedNodes=[Node||Node<-AllNodes,
			false==lists:member(Node,ActiveNodes)],
    {ok,StoppedNodes}.
    
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-define(TimeOut,20*1000).
create_appl(ApplSpec,PodNode)->
    create_appl(ApplSpec,PodNode,?TimeOut).

create_appl(ApplSpec,PodNode,TimeOut)->
    {ok,PodDir}=db_pod_desired_state:read(pod_dir,PodNode),
    % Delete and create ApplDir
    ApplDir=filename:join(PodDir,ApplSpec),
    rpc:call(PodNode,file,del_dir_r,[ApplDir],5000),
    ok=rpc:call(PodNode,file,make_dir,[ApplDir],5000),
    %% set application envs
    {ok,HostSpec}=db_pod_desired_state:read(host_spec,PodNode),
    {ok,ApplicationConfig}=db_host_spec:read(application_config,HostSpec),  
    _SetEnvResult=[rpc:call(PodNode,application,set_env,[[Config]],5000)||Config<-ApplicationConfig],
    {ok,PodApplGitPath}=db_appl_spec:read(gitpath,ApplSpec),
    Result=case do_load_start(ApplSpec,PodNode,PodApplGitPath,ApplDir,TimeOut) of
	       {error,Reason}->
		   rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,{error,Reason}]),
		   {error,Reason};
	       ok->
		  % {atomic,ok}=db_appl_instance:create(ClusterSpec,ApplSpec,PodNode,HostSpec,{date(),time()}),
		   {ok,LocalTypeList}=db_appl_spec:read(local_type,ApplSpec),
		   [rpc:call(PodNode,rd,add_local_resource,[LocalType,PodNode],5000)||LocalType<-LocalTypeList],
						%   [rd:add_target_resource_type(LocalType)||LocalType<-LocalTypeList],
		   {ok,TargetTypeList}=db_appl_spec:read(target_type,ApplSpec),
		   [rpc:call(PodNode,rd,add_target_resource_type,[TargetType],5000)||TargetType<-TargetTypeList],
		   rpc:call(PodNode,rd,trade_resources,[],5000),
		   timer:sleep(2000),
		   rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["application created ",ApplSpec,PodNode]]),
		   ok
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
do_load_start(ApplSpec,PodNode,PodApplGitPath,ApplDir,TimeOut)->
    Result= case rpc:call(PodNode,file,make_dir,[ApplDir],5000) of
		{error,Reason}->
		    rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,{error,Reason}]),
		    {error,Reason};
		ok->
		    case appl:git_clone_to_dir(PodNode,PodApplGitPath,ApplDir) of
			{error,Reason}->
			    rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,{error,Reason}]),
			    {error,Reason};
			{ok,_}->
			    {ok,PodApp}=db_appl_spec:read(app,ApplSpec),
			    ApplEbin=filename:join([ApplDir,"ebin"]),
			    Paths=[ApplEbin],
			    case appl:load(PodNode,PodApp,Paths) of
				{error,Reason}->
				    rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,{error,Reason}]),
				    {error,Reason}; 
				ok->
				    case appl:start(PodNode,PodApp,TimeOut) of
					{error,Reason}->
					    rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,{error,Reason}]),
					    {error,Reason};
					ok->
					    {ok,LocalTypeList}=db_appl_spec:read(local_type,ApplSpec),
					    [rpc:call(PodNode,rd,add_local_resource,[LocalType,PodNode],5000)||LocalType<-LocalTypeList],
						%   [rd:add_target_resource_type(LocalType)||LocalType<-LocalTypeList],
					    {ok,TargetTypeList}=db_appl_spec:read(target_type,ApplSpec),
					    [rpc:call(PodNode,rd,add_target_resource_type,[TargetType],5000)||TargetType<-TargetTypeList],
					    rpc:call(PodNode,rd,trade_resources,[],5000),
					    timer:sleep(2000),
					    ok
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
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_candidate_pods(ApplSpec,HostSpec)->
    % Candidates
    {ok,ActivePods}=pod_server:active_nodes(),
    Candidates=[PodNode||PodNode<-db_pod_desired_state:get_all_id(),
			 {ok,HostSpec}==db_pod_desired_state:read(host_spec,podNode),
			 {ok,ApplSpec}/=db_pod_desired_state:read(appl_spec,PodNode),
			 true==lists:member(PodNode,ActivePods)],
    prioritize(Candidates,[]).

prioritize([],Acc)->
    [PodNode||{_NumApplSpecs,PodNode}<-lists:keysort(1,Acc)];
prioritize([PodNode|T],Acc) ->
    {ok,ApplSpecList}=db_pod_desired_state:read(appl_spec,PodNode),
    NumApplSpecs=list_length:start(ApplSpecList),
    prioritize(T,[{NumApplSpecs,PodNode}|Acc]).
    
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
load_desired_state(ClusterSpec)->
    {ok,PodsHostList}=db_cluster_spec:read(pods,ClusterSpec),
    HostSpecList=[HostSpec||{_Num,HostSpec}<-PodsHostList],
    ApplDeploymentSpecInfoList=[db_appl_deployment:read(ApplDeploymentId)||ApplDeploymentId<-db_appl_deployment:get_all_id()],
    main_load_desired_state(ApplDeploymentSpecInfoList,HostSpecList,[]).

main_load_desired_state([],_HostSpecList,Acc)->
    Acc;
main_load_desired_state([ApplDeploymentSpec|T],HostSpecList,Acc) ->
    Result=load_desired_state(ApplDeploymentSpec,HostSpecList,[]),
    main_load_desired_state(T,HostSpecList,[Result|Acc]).

%%-- Affinity any_host    
load_desired_state({_SpecId,_ApplSpec,_Vsn,_ClusterSpec,0,any_host},_HostList,Acc)->
    Acc;
load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,N,any_host},[HostSpec|T],Acc)->
    PodNode=get_candidate_pods(ApplSpec,HostSpec),    
    Result=case db_pod_desired_state:add_appl_list(ApplSpec,PodNode) of
	       {aborted,Reason}->
		   Reason;
	       {atomic,ok}->
		   ok
	   end,
    RotatedHostList=lists:append(T,[HostSpec]),
    load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,N-1,any_host},RotatedHostList,[Result|Acc]);
		       
load_desired_state({_SpecId,_ApplSpec,_Vsn,_ClusterSpec,0,_Affinity},_HostList,Acc)->
    Acc;
load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,N,[WantedHostSpec|T]},HostList,Acc)->
    Result=case lists:member(WantedHostSpec,HostList) of
	       false->
		   {error,["ERROR: WantedHost not part of cluster hosts : ",WantedHostSpec,HostList]};
	       true->
		   PodNode=get_candidate_pods(ApplSpec,WantedHostSpec),    
		   {atomic,ok}=db_pod_desired_state:add_appl_list(ApplSpec,PodNode),
		   ok
	   end,
    RotatedWantedHostList=lists:append(T,[WantedHostSpec]),
    load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,N-1,RotatedWantedHostList},HostList,[Result|Acc]).
