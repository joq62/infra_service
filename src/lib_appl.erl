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
    AllNodes=db_pod_desired_state:get_all_id(),
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
    ApplDir=filename:join(PodDir,ApplSpec),
    rpc:call(PodNode,file,del_dir_r,[ApplDir],5000),
    ok=rpc:call(PodNode,file,make_dir,[ApplDir],5000),
    
    {ok,HostSpec}=db_pod_desired_state:read(host_spec,PodNode),
    {ok,ApplicationConfig}=db_host_spec:read(application_config,HostSpec),  
    _SetEnvResult=[rpc:call(PodNode,application,set_env,[[Config]],5000)||Config<-ApplicationConfig],
    {ok,PodApplGitPath}=db_appl_spec:read(gitpath,ApplSpec),
    Result=case do_load_start(ApplSpec,PodNode,PodApplGitPath,ApplDir,TimeOut) of
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
get_candidate_pods(SpecId,HostSpec,ClusterSpec)->
    {ok,ApplSpec}=db_appl_deployment:read(appl_spec,SpecId),
    RightHost=[PodNode||PodNode<-db_pod_desired_state:get_all_id(),
			{ok,HostSpec}==db_pod_desired_state:read(host_spec,PodNode)],
    NodeApplSpecList=[{PodNode,db_pod_desired_state:read(appl_spec_list,PodNode)}||PodNode<-RightHost],
    Candidates=[PodNode||{PodNode,{ok,ApplSpecList}}<-NodeApplSpecList,
			 false==lists:member(ApplSpec,ApplSpecList),
			 {ok,ClusterSpec}==db_pod_desired_state:read(cluster_spec,PodNode)],
    prioritize(Candidates,[]).

prioritize([],Acc)->
    [PodNode||{_NumApplSpecs,PodNode}<-lists:keysort(1,Acc)];
prioritize([PodNode|T],Acc) ->
    {ok,ApplSpecList}=db_pod_desired_state:read(appl_spec_list,PodNode),
    NumApplSpecs=list_length:start(ApplSpecList),
    prioritize(T,[{NumApplSpecs,PodNode}|Acc]).
 
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load_desired_state(ClusterSpec)->
    {ok,PodsHostList}=db_cluster_spec:read(pods,ClusterSpec),
  %  io:format("PodsHostList ~p~n",[{PodsHostList,?MODULE,?FUNCTION_NAME,?LINE}]),
    HostSpecList=[HostSpec||{_Num,HostSpec}<-PodsHostList],
    AllDeploymentId=db_appl_deployment:get_all_id(),
    ApplDeploymentSpecInfoList=[db_appl_deployment:read(ApplDeploymentId)||ApplDeploymentId<-AllDeploymentId,
			       {ok,ClusterSpec}==db_appl_deployment:read(cluster_spec,ApplDeploymentId)],
    _Result=main_load_desired_state(ApplDeploymentSpecInfoList,HostSpecList,[]),
 %   io:format("Result ~p~n",[{Result,?MODULE,?FUNCTION_NAME,?LINE}]).
    ok.

main_load_desired_state([],_HostSpecList,Acc)->
    Acc;
main_load_desired_state([ApplDeploymentSpec|T],HostSpecList,Acc) ->
    Result=load_desired_state(ApplDeploymentSpec,HostSpecList,[]),
    main_load_desired_state(T,HostSpecList,[Result|Acc]).

%%-- Affinity on each pod each_pod
load_desired_state({_SpecId,_ApplSpec,_Vsn,_ClusterSpec,_,each_pod},[],Acc)->
    Acc;
load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,1,each_pod},[HostSpec|T],Acc)->
    RightHost=[PodNode||PodNode<-db_pod_desired_state:get_all_id(),
			{ok,HostSpec}==db_pod_desired_state:read(host_spec,PodNode)],
    NodeApplSpecList=[{PodNode,db_pod_desired_state:read(appl_spec_list,PodNode)}||PodNode<-RightHost],
    Candidates=[PodNode||{PodNode,{ok,ApplSpecList}}<-NodeApplSpecList,
			 false==lists:member(ApplSpec,ApplSpecList),
			 {ok,ClusterSpec}==db_pod_desired_state:read(cluster_spec,PodNode)],
    AddResult=[{db_pod_desired_state:add_appl_list(ApplSpec,PodNode),ApplSpec,PodNode,HostSpec}||PodNode<-Candidates],
    ErrorResult=[{error,["ERROR: Aborted ApplSpec,PodNode,HostSpec : ",Reason,ApplSpec,PodNode,HostSpec]}||
		    {{aborted,Reason},ApplSpec,PodNode,HostSpec}<-AddResult],
    OkResult=[ok||{atomic,ok}<-AddResult],
    NewAcc=lists:append([OkResult,ErrorResult,Acc]),
    load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,1,each_pod},T,NewAcc);


%%-- Affinity any_host    
load_desired_state({_SpecId,_ApplSpec,_Vsn,_ClusterSpec,0,any_host},_HostList,Acc)->
    Acc;
load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,N,any_host},[HostSpec|T],Acc)->
  %  io:format("ApplSpec,WantedHostSpec ~p~n",[{ApplSpec,HostSpec,?MODULE,?FUNCTION_NAME}]),
    Result=case get_candidate_pods(SpecId,HostSpec,ClusterSpec) of
	       []->
		   {error,["ERROR: No candidates  ",ApplSpec,HostSpec]};
	       [PodNode|_]->
		   case db_pod_desired_state:add_appl_list(ApplSpec,PodNode) of
		       {aborted,Reason}->
			   {error,["ERROR: Aborted ApplSpec,PodNode,HostSpec : ",Reason,ApplSpec,PodNode,HostSpec]};
		       {atomic,ok}->
			%   io:format("Ok  ~p~n",[{ApplSpec,PodNode,?MODULE,?FUNCTION_NAME}]),
			   ok
		   end
	   end,
    RotatedHostList=lists:append(T,[HostSpec]),
    load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,N-1,any_host},RotatedHostList,[Result|Acc]);
		       
load_desired_state({_SpecId,_ApplSpec,_Vsn,_ClusterSpec,0,_Affinity},_HostList,Acc)->
    Acc;
load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,N,[HostSpec|T]},HostList,Acc)->
  %  io:format("ApplSpec,WantedHostSpec ~p~n",[{ApplSpec,WantedHostSpec,?MODULE,?FUNCTION_NAME}]),
    Result=case lists:member(HostSpec,HostList) of
	       false->
		   {error,["ERROR: Host not part of cluster hosts : ",HostSpec,HostList]};
	       true->
		   case get_candidate_pods(SpecId,HostSpec,ClusterSpec) of
		       []->
			   {error,["ERROR: No candidates  ",ApplSpec,HostSpec]};
		       [PodNode|_]->				   
			   case db_pod_desired_state:add_appl_list(ApplSpec,PodNode) of
			       {aborted,Reason}->
				   {error,["ERROR: Aborted ApplSpec,PodNode,HostSpec : ",Reason,ApplSpec,PodNode,HostSpec]};
			       {atomic,ok}->
			%	   io:format("Ok  ~p~n",[{ApplSpec,PodNode,?MODULE,?FUNCTION_NAME}]),
				   ok
			   end
		   end
	   end,
    RotatedWantedHostList=lists:append(T,[HostSpec]),
    load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,N-1,RotatedWantedHostList},HostList,[Result|Acc]).