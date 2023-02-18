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
    
    Result=case sd:call(db_etcd,db_appl_desired_state,get_all_id,[],5000) of
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
  
    A1=[{PodNode,sd:call(db_etcd,db_pod_desired_state,read,[appl_spec_list,PodNode],5000)}||PodNode<-sd:call(db_etcd,db_pod_desired_state,get_all_id,[],5000)],
  %  io:format("A1 ~p~n",[{A1,?MODULE,?FUNCTION_NAME,?LINE}]),
    A2=[{PodNode,ApplList}||{PodNode,{ok,ApplList}}<-A1],
    PodApplSpecAppList=lists:append([pod_app_list(PodApplSpecList,[])||PodApplSpecList<-A2]),
    {ok,StoppedAppls}=stopped_appls(),
    ActiveAppls=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-PodApplSpecAppList,
					false==lists:member({PodNode,ApplSpec,App},StoppedAppls)],
    {ok,ActiveAppls}.
    %%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
stopped_appls()->
    A1=[{PodNode,sd:call(db_etcd,db_pod_desired_state,read,[appl_spec_list,PodNode],5000)}||PodNode<-sd:call(db_etcd,db_pod_desired_state,get_all_id,[],5000)],
%    io:format("A1 ~p~n",[{A1,?MODULE,?FUNCTION_NAME,?LINE}]),
    A2=[{PodNode,ApplList}||{PodNode,{ok,ApplList}}<-A1],
    PodApplSpecAppList=lists:append([pod_app_list(PodApplSpecList,[])||PodApplSpecList<-A2]),
    
    StoppedAppls=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-PodApplSpecAppList,
					  false==is_app_running(App,PodNode)],
    {ok,StoppedAppls}.
    

is_app_running(App,PodNode)->
    Result=case rpc:call(PodNode,application,which_applications,[],5000) of
	       {badrpc,_}->
		   false;
	       Applications->
		   lists:keymember(App,1,Applications)
	   end,
    Result.


    
    
pod_app_list({_PodNode,[]},Acc)->
    Acc;
pod_app_list({PodNode,[ApplSpec|T]},Acc)->
   {ok,App}=sd:call(db_etcd,db_appl_spec,read,[app,ApplSpec],5000),
    NewAcc=[{PodNode,ApplSpec,App}|Acc],
    pod_app_list({PodNode,T},NewAcc).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-define(TimeOut,20*1000).
create_appl(ApplSpec,PodNode)->
    create_appl(ApplSpec,PodNode,?TimeOut).

create_appl(ApplSpec,PodNode,TimeOut)->
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG ApplSpec,PodNode :", ApplSpec,PodNode,?MODULE,?LINE]]),
   Result=case sd:call(db_etcd,db_pod_desired_state,read,[pod_dir,PodNode],5000) of
	      {error,Reason}->
		  sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG Error Reason :", Reason,?MODULE,?LINE]]),
		  {error,Reason};
	      {ok,PodDir}->
		  sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG Ok PodDir :", PodDir,?MODULE,?LINE]]),
		  ApplDir=filename:join(PodDir,ApplSpec),
		  rpc:call(PodNode,file,del_dir_r,[ApplDir],5000),
		  case rpc:call(PodNode,file,make_dir,[ApplDir],5000) of
		      {badrpc,Reason}->
			  sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG Error Reason :", Reason,?MODULE,?LINE]]),
			  {error,["Error during do_start : ",badrpc,Reason,ApplSpec,PodNode,?MODULE,?FUNCTION_NAME,?LINE]};
		      {error,Reason}->
			  sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG Error Reason :", Reason,?MODULE,?LINE]]),
			  {error,Reason};
		      ok->
			  case sd:call(db_etcd,db_pod_desired_state,read,[host_spec,PodNode],5000) of
			        {error,Reason}->
				  sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG Error Reason :", Reason,?MODULE,?LINE]]),
				  {error,Reason};
			      {ok,HostSpec}->
				  case sd:call(db_etcd,db_host_spec,read,[application_config,HostSpec],5000) of
				      {error,Reason}->
					  sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG Error Reason :", Reason,?MODULE,?LINE]]),
					  {error,Reason};
				      {ok,ApplicationConfig}->
					  _SetEnvResult=[rpc:call(PodNode,application,set_env,[[Config]],5000)||Config<-ApplicationConfig],
					  {ok,PodApplGitPath}=db_appl_spec:read(gitpath,ApplSpec),
					  case do_load_start(ApplSpec,PodNode,PodApplGitPath,ApplDir,TimeOut) of
					      {error,Reason}->
						  sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG Error Reason :", Reason,?MODULE,?LINE]]),
						  {error,["Error during do_start : ",Reason,ApplSpec,PodNode,?MODULE,?FUNCTION_NAME,?LINE]};
					      ok->
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
do_load_start(ApplSpec,PodNode,PodApplGitPath,ApplDir,TimeOut)->
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG ApplSpec,PodNode,PodApplGitPath,ApplDir :", 
							      ApplSpec,PodNode,PodApplGitPath,ApplDir,?MODULE,?LINE]]),
    Result= case appl:git_clone_to_dir(PodNode,PodApplGitPath,ApplDir) of
		{error,Reason}->
		    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG Error Reason :", Reason,?MODULE,?LINE]]),
		    {error,["Error when cloning : ", Reason,PodNode,PodApplGitPath,ApplDir,?MODULE,?FUNCTION_NAME,?LINE]};
		{ok,_}->
		    {ok,PodApp}=sd:call(db_etcd,db_appl_spec,read,[app,ApplSpec],5000),
		    ApplEbin=filename:join([ApplDir,"ebin"]),
		    Paths=[ApplEbin],
		    case appl:load(PodNode,PodApp,Paths) of
			{error,Reason}->
			    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG Error Reason :", Reason,?MODULE,?LINE]]),
			    {error,["Error when loading application : ",Reason,PodNode,PodApp,Paths,?MODULE,?FUNCTION_NAME,?LINE]}; 
			ok->
			    case appl:start(PodNode,PodApp,TimeOut) of
				{error,Reason}->
				    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG Error Reason :", Reason,?MODULE,?LINE]]),
				    {error,Reason};
				ok->
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
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
get_candidate_pods(SpecId,HostSpec,ClusterSpec)->
    {ok,ApplSpec}=sd:call(db_etcd,db_appl_deployment,read,[appl_spec,SpecId],5000),
    RightHost=[PodNode||PodNode<-sd:call(db_etcd,db_pod_desired_state,get_all_id,[],5000),
			{ok,HostSpec}==sd:call(db_etcd,db_pod_desired_state,read,[host_spec,PodNode],5000)],
    NodeApplSpecList=[{PodNode,sd:call(db_etcd,db_pod_desired_state,read,[appl_spec_list,PodNode],5000)}||PodNode<-RightHost],
    Candidates=[PodNode||{PodNode,{ok,ApplSpecList}}<-NodeApplSpecList,
			 false==lists:member(ApplSpec,ApplSpecList),
			 {ok,ClusterSpec}==sd:call(db_etcd,db_pod_desired_state,read,[cluster_spec,PodNode],5000)],
    prioritize(Candidates,[]).

prioritize([],Acc)->
    [PodNode||{_NumApplSpecs,PodNode}<-lists:keysort(1,Acc)];
prioritize([PodNode|T],Acc) ->
    {ok,ApplSpecList}=sd:call(db_etcd,db_pod_desired_state,read,[appl_spec_list,PodNode],5000),
    NumApplSpecs=list_length:start(ApplSpecList),
    prioritize(T,[{NumApplSpecs,PodNode}|Acc]).
 
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
load_desired_state(ClusterSpec)->
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["load_desired_state : ",ClusterSpec,?MODULE,?LINE]]),
    {ok,PodsHostList}=sd:call(db_etcd,db_cluster_spec,read,[pods,ClusterSpec],5000),
    
  %  io:format("PodsHostList ~p~n",[{PodsHostList,?MODULE,?FUNCTION_NAME,?LINE}]),
    HostSpecList=[HostSpec||{_Num,HostSpec}<-PodsHostList],
    AllDeploymentId=sd:call(db_etcd,db_appl_deployment,get_all_id,[],5000),
    ApplDeploymentSpecInfoList=[sd:call(db_etcd,db_appl_deployment,read,[ApplDeploymentId],5000)||ApplDeploymentId<-AllDeploymentId,
			       {ok,ClusterSpec}==sd:call(db_etcd,db_appl_deployment,read,[cluster_spec,ApplDeploymentId],5000)],
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ApplDeploymentSpecInfoList : ",ApplDeploymentSpecInfoList,?MODULE,?LINE]]),
    Result=main_load_desired_state(ApplDeploymentSpecInfoList,HostSpecList,[]),
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Result : ",Result,?MODULE,?LINE]]),
 %   io:format("Result ~p~n",[{Result,?MODULE,?FUNCTION_NAME,?LINE}]).
    ok.

main_load_desired_state([],_HostSpecList,Acc)->
    Acc;
main_load_desired_state([ApplDeploymentSpec|T],HostSpecList,Acc) ->
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,[" : ",?MODULE,?LINE]]),
    Result=load_desired_state(ApplDeploymentSpec,HostSpecList,[]),
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Result : ",Result,?MODULE,?LINE]]),
    main_load_desired_state(T,HostSpecList,[Result|Acc]).

%%-- Affinity on each pod each_pod
load_desired_state({_SpecId,_ApplSpec,_Vsn,_ClusterSpec,_,each_pod},[],Acc)->
    Acc;
load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,1,each_pod},[HostSpec|T],Acc)->
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ApplSpec : ",ApplSpec,?MODULE,?LINE]]),
    RightHost=[PodNode||PodNode<-sd:call(db_etcd,db_pod_desired_state,get_all_id,[],5000),
			{ok,HostSpec}==sd:call(db_etcd,db_pod_desired_state,read,[host_spec,PodNode],5000)],
    NodeApplSpecList=[{PodNode,sd:call(db_etcd,db_pod_desired_state,read,[appl_spec_list,PodNode],5000)}||PodNode<-RightHost],
    Candidates=[PodNode||{PodNode,{ok,ApplSpecList}}<-NodeApplSpecList,
			 false==lists:member(ApplSpec,ApplSpecList),
			 {ok,ClusterSpec}==sd:call(db_etcd,db_pod_desired_state,read,[cluster_spec,PodNode],5000)],
    AddResult=[{sd:call(db_etcd,db_pod_desired_state,add_appl_list,[ApplSpec,PodNode],5000),ApplSpec,PodNode,HostSpec}||PodNode<-Candidates],
    ErrorResult=[{error,["ERROR: Aborted ApplSpec,PodNode,HostSpec : ",Reason,ApplSpec,PodNode,HostSpec]}||
		    {{aborted,Reason},ApplSpec,PodNode,HostSpec}<-AddResult],
    OkResult=[{ok,ApplSpec,PodNode,HostSpec}||{{atomic,ok},ApplSpec,PodNode,HostSpec}<-AddResult],
    NewAcc=lists:append([OkResult,ErrorResult,Acc]),
    load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,1,each_pod},T,NewAcc);


%%-- Affinity any_host    
load_desired_state({_SpecId,_ApplSpec,_Vsn,_ClusterSpec,0,any_host},_HostList,Acc)->
    Acc;
load_desired_state({SpecId,ApplSpec,_Vsn,ClusterSpec,N,any_host},[HostSpec|T],Acc)->
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ApplSpec : ",ApplSpec,?MODULE,?LINE]]),
  %  io:format("ApplSpec,WantedHostSpec ~p~n",[{ApplSpec,HostSpec,?MODULE,?FUNCTION_NAME}]),
    Result=case get_candidate_pods(SpecId,HostSpec,ClusterSpec) of
	       []->
		   {error,["ERROR: No candidates  ",ApplSpec,HostSpec]};
	       [PodNode|_]->
		   case sd:call(db_etcd,db_pod_desired_state,add_appl_list,[ApplSpec,PodNode],5000) of
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
			   case sd:call(db_etcd,db_pod_desired_state,add_appl_list,[ApplSpec,PodNode],5000) of
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
