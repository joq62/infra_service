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
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,
				 ["DEBUG config,ClusterSpec  : ",ClusterSpec,?MODULE,?LINE]]),
    Result=case parent_server:load_desired_state(ClusterSpec) of
	        {error,Reason}->
		   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,
						["parent_server:load_desired_state  : ",Reason,?MODULE,?LINE]]),
		   {error,Reason};
	       ok ->
		   sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,
						["parent_server:load_desired_state  : ",ok,?MODULE,?LINE]]),
		   case pod_server:load_desired_state(ClusterSpec) of
		       {error,Reason}->
			   sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,
							["pod_server:load_desired_state  : ",Reason,?MODULE,?LINE]]),
			   {error,Reason};
		       ok ->
			   sd:cast(nodelog,nodelog,log,
				   [notice,?MODULE_STRING,?LINE,["pod_server:load_desired_state  : ",ok,?MODULE,?LINE]]),
			   case appl_server:load_desired_state(ClusterSpec) of
			       {error,Reason}->
				   sd:cast(nodelog,nodelog,log,
					   [warning,?MODULE_STRING,?LINE,["appl_server:load_desired_state  : ",Reason,?MODULE,?LINE]]),
				   {error,Reason};
			       ok ->
				   sd:cast(nodelog,nodelog,log,
					   [notice,?MODULE_STRING,?LINE,["appl_server:load_desired_state  : ",ok,?MODULE,?LINE]]),
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

orchistrate(ClusterSpec)->
    orchistrate(ClusterSpec,?SleepInterval).
orchistrate(ClusterSpec,SleepInterval)->
    timer:sleep(SleepInterval),
    ResultStartParents=rpc:call(node(),?MODULE,start_parents,[],15*1000),
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultStartParents  : ",ResultStartParents,?MODULE,?LINE]]),

    ResultStartPods=rpc:call(node(),?MODULE,start_pods,[],60*1000),
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultStartPods  : ",ResultStartPods,?MODULE,?LINE]]),
    

    ResultStartInfraAppls=rpc:call(node(),?MODULE,start_infra_appls,[ClusterSpec],60*1000),
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultStartInfraAppls  : ",ResultStartInfraAppls,?MODULE,?LINE]]),
    ResultStartUserAppls=rpc:call(node(),?MODULE,start_user_appls,[],60*1000), 

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
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: function start : ",node(),?MODULE,?LINE]]),
    {ok,StoppedParents}=parent_server:stopped_nodes(),
    [parent_server:create_node(Parent)||Parent<-StoppedParents],
    {ok,ActiveParents}=parent_server:active_nodes(),
    _R1=[{net_adm:ping(Pod1),rpc:call(Pod1,net_adm,ping,[Pod2],5000)}||Pod1<-ActiveParents,
								   Pod2<-ActiveParents,
								   Pod1/=Pod2],	
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: StoppedParents : ",StoppedParents,node(),?MODULE,?LINE]]),
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: ActiveParents : ",ActiveParents,node(),?MODULE,?LINE]]),
					
    StoppedParents.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_pods()->
     sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: function start : ",node(),?MODULE,?LINE]]),
    {ok,Stopped}=pod_server:stopped_nodes(),
    [pod_server:create_node(Pod)||Pod<-Stopped],
    {ok,Active}=pod_server:active_nodes(),
    _R1=[{net_adm:ping(Pod1),rpc:call(Pod1,net_adm,ping,[Pod2],5000)}||Pod1<-Active,
								   Pod2<-Active,
								   Pod1/=Pod2],	
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: Stopped : ",Stopped,node(),?MODULE,?LINE]]),
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: Active: ",Active,node(),?MODULE,?LINE]]),
    Stopped.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_infra_appls(ClusterSpec)->   
    {ok,StoppedApplInfoLists}=appl_server:stopped_appls(),    
    R_Nodelog=[{create_infra_appl({PodNode,ApplSpec,App},ClusterSpec),ApplSpec,PodNode}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
											 nodelog==App],
    R_db_etcd=[{create_infra_appl({PodNode,ApplSpec,App},ClusterSpec),ApplSpec,PodNode}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
											 db_etcd==App],
    R_infra_service=[{create_infra_appl({PodNode,ApplSpec,App},ClusterSpec),ApplSpec,PodNode}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
											       infra_service==App],

    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: StoppedApplInfoLists : ",StoppedApplInfoLists,node(),?MODULE,?LINE]]),
    
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG: R_Nodelog : ",R_Nodelog,node(),?MODULE,?LINE]]),
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG:  R_db_etcd : ", R_db_etcd,node(),?MODULE,?LINE]]),
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DBG:  R_infra_service : ", R_infra_service,node(),?MODULE,?LINE]]),
    [{nodelog,R_Nodelog},{db_etcd,R_db_etcd},{infra_service,R_infra_service}].

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_pods_based_appl(ApplSpec)->
    AllPodsApplSpecsToStart=[{PodNode,db_pod_desired_state:read(appl_spec_list,PodNode)}||PodNode<-db_pod_desired_state:get_all_id(),
											  pang==net_adm:ping(PodNode)],
    PodsToStart=[PodNode||{PodNode,{ok,ApplSpecList}}<-AllPodsApplSpecsToStart,
			  lists:member(ApplSpec,ApplSpecList)],
 %   io:format("PodsToStart, ApplSpec ~p~n",[{PodsToStart,ApplSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    [{create_pod(PodNode),PodNode,ApplSpec}||PodNode<-PodsToStart].

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_pod(PodNode)->
    {ok,ParentNode}=sd:call(db_etcd,db_pod_desired_state,read,[parent_node,PodNode],5000),
    {ok,NodeName}=sd:call(db_etcd,db_pod_desired_state,read,[node_name,PodNode],5000),
    {ok,PodDir}=sd:call(db_etcd,db_pod_desired_state,read,[pod_dir,PodNode],5000),
    {ok,PaArgsList}=sd:call(db_etcd,db_pod_desired_state,read,[pa_args_list,PodNode],5000),
    {ok,EnvArgs}=sd:call(db_etcd,db_pod_desired_state,read,[env_args,PodNode],5000),
    pod_server:create_pod(ParentNode,NodeName,PodDir,PaArgsList,EnvArgs).

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
    {ok,StoppedApplInfoLists}=appl_server:stopped_appls(),
    StoppedUserApplications=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
						     pod/=App,
						     db_etcd/=App,
						     nodelog/=App,
						     infra_service/=App],
    Result=[{error,Reason}||{error,Reason}<-create_appl(StoppedUserApplications,[])],
    Result.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
ensure_right_cookie(ClusterSpec)->
    {ok,Cookie}=sd:call(db_etcd,db_cluster_spec,read,[cookie,ClusterSpec],5000),
    erlang:set_cookie(node(),list_to_atom(Cookie)),
    
    ok.


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
    Result=appl_server:create_appl(ApplSpec,PodNode),
    create_appl(T,[{Result,PodNode,ApplSpec,App}|Acc]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
