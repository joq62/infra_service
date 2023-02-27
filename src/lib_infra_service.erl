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
-define(SleepInterval,60*1000).
%% API
-export([
	 create_appl/1,
	 create_infra_appl/2,
	 create_pods_based_appl/1,
	 init_servers/1,
	 ensure_right_cookie/1,
	 orchistrate/1,
	 orchistrate/2,
	 start_parents/0,
	 start_pods/0,
	 start_infra_appls/1,
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
init_servers(ClusterSpec)->
    Result=case rpc:call(node(),parent_server,load_desired_state,[ClusterSpec],30*10000) of
	       ok ->
		   case rpc:call(node(),pod_server,load_desired_state,[ClusterSpec],30*1000) of
		       ok ->
			   case rpc:call(node(),appl_server,load_desired_state,[ClusterSpec],30*1000) of
			       ok ->
				   ok;
			       Reason->
				   sd:cast(nodelog,nodelog,log,
					   [warning,?MODULE_STRING,?LINE,["appl_server:load_desired_state  : ",Reason,?MODULE,?LINE]]),
				   {error,Reason}
			   end;
		       Reason->
			   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,
							["pod_server:load_desired_state  : ",Reason,?MODULE,?LINE]]),
			   {error,Reason}
		   end;			       
	       Reason->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,
						["parent_server:load_desired_state  : ",Reason,?MODULE,?LINE]]),
		   {error,Reason}
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

orchistrate(ClusterSpec)->
    orchistrate(ClusterSpec,?SleepInterval).
orchistrate(ClusterSpec,SleepInterval)->
 %   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG orchistrate  : ",time(),?MODULE,?LINE]]),
    timer:sleep(SleepInterval),
%    ResultStartParents=debug1,
    ResultStartParents=rpc:call(node(),?MODULE,start_parents,[],15*1000),
%    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultStartParents  : ",ResultStartParents,?MODULE,?LINE]]),
 %   ResultStartPods=debug2,
    ResultStartPods=rpc:call(node(),?MODULE,start_pods,[],60*1000),
%    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultStartPods  : ",ResultStartPods,?MODULE,?LINE]]),
    
 %   ResultStartInfraAppls=debug3,
    ResultStartInfraAppls=rpc:call(node(),?MODULE,start_infra_appls,[ClusterSpec],60*1000),
%    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultStartInfraAppls  : ",ResultStartInfraAppls,?MODULE,?LINE]]),
%    ResultStartUserAppls=debug4,
    ResultStartUserAppls=rpc:call(node(),?MODULE,start_user_appls,[],60*1000), 
%    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultStartUserAppls  : ",ResultStartUserAppls,?MODULE,?LINE]]),


    rpc:cast(node(),infra_service,orchistrate_result,[ResultStartParents,
						      ResultStartPods,
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
start_parents()->
    Result=case rpc:call(node(),parent_server,stopped_nodes,[],10*1000) of
	       {ok,[]}->
		   {ok,[]};
	       {ok,StoppedParents}->
		   io:format("StoppedParents  ~p~n",[{StoppedParents,?MODULE,?LINE}]),
	%	   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: StoppedParents : ",StoppedParents,node(),?MODULE,?LINE]]),    
		   _CreateResult=[{rpc:call(node(),parent_server,create_node,[Parent],25*1000),Parent}||Parent<-StoppedParents],
	%	   [sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error Creating parent node :", CreateRes,ParentNode,?MODULE,?LINE]])||
	%	       {CreateRes,ParentNode}<-CreateResult,
	%	       {ok,ParentNode}/=CreateRes],
		   case rpc:call(node(),parent_server,active_nodes,[],20*1000) of
		       {ok,ActiveParents}->
	%		   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: ActiveParents : ",ActiveParents,node(),?MODULE,?LINE]]),
			   R1=[{net_adm:ping(Pod1),rpc:call(Pod1,net_adm,ping,[Pod2],5000)}||Pod1<-ActiveParents,
											      Pod2<-ActiveParents,
											      Pod1/=Pod2],
			   {ok,ActiveParents};
		       
		       Reason->
			   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,
							["parent_server:active_nodes  : ",Reason,?MODULE,?LINE]]),
			   {error,Reason}
		   end;
	       Reason->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,
						["parent_server:stopped_nodes  : ",Reason,?MODULE,?LINE]]),
		   {error,Reason}
	       end,
			   
  %  sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: StoppedParents : ",StoppedParents,node(),?MODULE,?LINE]]),
  %  sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: ActiveParents : ",ActiveParents,node(),?MODULE,?LINE]]),
		       
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_pods()->
  %   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: function start : ",node(),?MODULE,?LINE]]),
    Result=case rpc:call(node(),pod_server,stopped_nodes,[],25*1000) of
	       {ok,[]}->
		   ok;
	       {ok,Stopped}->
		   CreateResult=[{rpc:call(node(),pod_server,create_node,[Pod],25*1000),Pod}||Pod<-Stopped],
	%	   [sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error Creating pod node :", CreateRes,Pod,?MODULE,?LINE]])||
	%	       {CreateRes,Pod}<-CreateResult,
	%	       {ok,Pod}/=CreateRes],
	%	   [sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["OK Creating pod node :",Pod,?MODULE,?LINE]])||
	%	       {ok,Pod}<-CreateResult],
		   _CommonStart=[{rpc:call(node(),appl_server,create_appl,["common",Pod],25*1000),Pod}||{ok,Pod}<-CreateResult],
		   _SdStart=[{rpc:call(node(),appl_server,create_appl,["sd",Pod],25*1000),Pod}||{ok,Pod}<-CreateResult],
		   case rpc:call(node(),pod_server,active_nodes,[],15*1000) of
		       {ok,Active}->
			   _R1=[{net_adm:ping(Pod1),rpc:call(Pod1,net_adm,ping,[Pod2],5000)}||Pod1<-Active,
											      Pod2<-Active,
											      Pod1/=Pod2],
			   {ok,Active,Stopped};
		       Reason->
			   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,
							["parent_server:active_nodes  : ",Reason,?MODULE,?LINE]]),
			   {error,Reason}
		   end;
	       Reason->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,
						["parent_server:stopped_nodes  : ",Reason,?MODULE,?LINE]]),
		   {error,Reason}
	   end,
    Result.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_infra_appls(ClusterSpec)->   
   Result=case rpc:call(node(),appl_server,stopped_appls,[],30*1000) of
	      {ok,[]}->
		  ok;
	      {ok,StoppedApplInfoLists}->
	%	  sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: StoppedApplInfoLists : ",time(),StoppedApplInfoLists,?MODULE,?LINE]]),
		  R_Nodelog=[{create_infra_appl({PodNode,ApplSpec,App},ClusterSpec),ApplSpec,PodNode}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
												       nodelog==App],
		  [sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultCreate nodelog :",ResultCreate,?MODULE,?LINE]])||
		      ResultCreate<- R_Nodelog],
		  
		  R_db_etcd=[{create_infra_appl({PodNode,ApplSpec,App},ClusterSpec),ApplSpec,PodNode}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
											 db_etcd==App],
		  [sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultCreate db_etcd :",ResultCreate,?MODULE,?LINE]])||
		      ResultCreate<-R_db_etcd],

		  R_infra_service=[{create_infra_appl({PodNode,ApplSpec,App},ClusterSpec),ApplSpec,PodNode}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
													     infra_service==App],
		  [sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultCreate db_etcd :",ResultCreate,?MODULE,?LINE]])||
		      ResultCreate<-R_infra_service],
		  [{nodelog,R_Nodelog},{db_etcd,R_db_etcd},{infra_service,R_infra_service}];
	      Reason->
		  {error,[Reason,?MODULE,?LINE]}
	  end,
    Result.


		  

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_pods_based_appl(ApplSpec)->
    Result=case sd:call(db_etcd,db_pod_desired_state,get_all_id,[],10*1000) of
	       {badrpc,Reason}->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["db_pod_desired_state,get_all_id : ",badrpc,Reason]]),
		   {error,[badrpc,Reason,?MODULE,?LINE]};
	       []->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["db_pod_desired_state,get_all_id : ",no_pods]]),
		   {error,[no_pods,?MODULE,?LINE]};
	       Pods->
		   io:format("DBG:  Pods ~p~n",[{Pods,?MODULE,?LINE}]), 
		   AllPodsApplSpecsToStart=[{PodNode,sd:call(db_etcd,db_pod_desired_state,read,[appl_spec_list,PodNode],5*1000)}||PodNode<-Pods,
																  pang==net_adm:ping(PodNode)],
		   io:format("DBG:  AllPodsApplSpecsToStart ~p~n",[{AllPodsApplSpecsToStart,?MODULE,?LINE}]), 
		   PodsToStart=[PodNode||{PodNode,{ok,ApplSpecList}}<-AllPodsApplSpecsToStart,
					 lists:member(ApplSpec,ApplSpecList)],
		   io:format("DBG:  PodsToStart ~p~n",[{PodsToStart,?MODULE,?LINE}]), 
		   [{create_pod(PodNode),PodNode,ApplSpec}||PodNode<-PodsToStart]
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_pod(PodNode)->
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
						   rpc:call(node(),pod_server,create_pod,[ParentNode,NodeName,PodDir,PaArgsList,EnvArgs],25*1000);
					       Reason ->
						   {error,[Reason,env_args,?MODULE,?LINE]}
					   end;
				       Reason ->
					   {error,[Reason,pa_args_list,?MODULE,?LINE]}
				   end;
			       Reason ->
				   {error,[Reason,pod_dir,?MODULE,?LINE]}
			   end;
		       Reason ->
			   {error,[Reason,node_name,?MODULE,?LINE]}
		   end;
	       Reason ->
		   {error,[Reason,parent_node,?MODULE,?LINE]}
	   end,
    Result.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_infra_appl({PodNode,ApplSpec,nodelog},_ClusterSpec)->
    Result= case create_appl([{PodNode,ApplSpec,nodelog}]) of
		{error,Reason}->
		    {error,Reason};
		[{ok,PodNode,ApplSpec,nodelog}]->
		    case sd:call(db_etcd,db_pod_desired_state,read,[pod_dir,PodNode],5000) of
			{badrpc,Reason}->
			    {error,[badrpc,Reason]};
			{error,Reason}->
			    {error,Reason};
			{ok,PodDir}->
			    PathLogDir=filename:join(PodDir,?LogDir),
			    rpc:call(PodNode,file,del_dir_r,[PathLogDir],5000),
			    case rpc:call(PodNode,file,make_dir,[PathLogDir],5000) of
				{Error,Reason}->
				    {Error,Reason};
				ok->
				    PathLogFile=filename:join([PathLogDir,?LogFileName]),
				    case rpc:call(PodNode,nodelog,config,[PathLogFile],5000) of
					{Error,Reason}->
					    {Error,Reason};
					ok->	
					    ok
				    end
			    end
		    end
	    end,
    Result;
    
 
create_infra_appl({PodNode,ApplSpec,db_etcd},_ClusterSpec) ->
    Result= case create_appl([{PodNode,ApplSpec,db_etcd}]) of
		{error,Reason}->
		    {error,Reason};
		[{ok,PodNode,ApplSpec,db_etcd}]->
		    case rpc:call(PodNode,db_etcd,config,[],5000) of
			{Error,Reason}->
			    {Error,Reason};
			ok->
			    ok
		    end
	    end,
    Result;
create_infra_appl({PodNode,ApplSpec,infra_service},ClusterSpec) ->
    
    Result= case create_appl([{PodNode,ApplSpec,infra_service}]) of
		{error,Reason}->
		    {error,Reason};
		[{ok,PodNode,ApplSpec,infra_service}]->
		    case rpc:call(PodNode,infra_service,config,[ClusterSpec],5*5000) of
			{Error,Reason}->
			    {Error,Reason};
			ok->
			    ok
		    end
	    end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_user_appls()->
    Result=case rpc:call(node(),appl_server,stopped_appls,[],15*1000) of
	       {ok,[]}->
		   ok;
	       {ok,StoppedApplInfoLists}->			  
		   StoppedUserApplications=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
								    common/=App,
								    sd/=App,
								    db_etcd/=App,
								    nodelog/=App,
								    infra_service/=App],
		   ApplCreateResult=create_appl(StoppedUserApplications,[]),
		   [sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Create start_user_appls Result :", CreateResult,?MODULE,?LINE]])||
		       CreateResult<-ApplCreateResult];
	       Reason->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error Create start_user_appls Result :", Reason,?MODULE,?LINE]]),
		   {error,["appl_server,stopped_appls ",Reason,?MODULE,?LINE]}
	   end,
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ensure_right_cookie(ClusterSpec)->
    Result=case sd:call(db_etcd,db_cluster_spec,read,[cookie,ClusterSpec],5000) of
	       {ok,Cookie}->
		   erlang:set_cookie(node(),list_to_atom(Cookie));
	       Reason->
		   {error,["db_etcd,db_cluster_spec,read,[cookie, ",Reason,?MODULE,?LINE]}
	   end,
    Result.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_appl(ApplSpecInfoList)->
    create_appl(ApplSpecInfoList,[]).
create_appl([],Acc)->
    Acc;
create_appl([{PodNode,ApplSpec,App}|T],Acc)->
    Result=rpc:call(node(),appl_server,create_appl,[ApplSpec,PodNode],25*1000),
%    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG PResult :", Result,?MODULE,?LINE]]),
    create_appl(T,[{Result,PodNode,ApplSpec,App}|Acc]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
