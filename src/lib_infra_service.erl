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
	 create_infra_appl/1,
	 create_pods_based_appl/1,
	 init_servers/1,
	 ensure_right_cookie/1,
	 orchistrate/0,
	 orchistrate/1,
	 start_parents/0,
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

orchistrate()->
    orchistrate(?SleepInterval).
orchistrate(SleepInterval)->
    timer:sleep(SleepInterval),
    ResultStartParentPods=rpc:call(node(),?MODULE,start_parents_pods,[],15*1000),
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultStartParentPods  : ",ResultStartParentPods,?MODULE,?LINE]]),

    ResultStartInfraAppls=rpc:call(node(),?MODULE,start_infra_appls,[],60*1000),
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ResultStartInfraAppls  : ",ResultStartInfraAppls,?MODULE,?LINE]]),
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
start_parents()->
    {ok,StoppedParents}=parent_server:stopped_nodes(),
    [parent_server:create_node(Parent)||Parent<-StoppedParents],
    {ok,ActiveParents}=parent_server:active_nodes(),
    _R1=[{net_adm:ping(Pod1),rpc:call(Pod1,net_adm,ping,[Pod2],5000)}||Pod1<-ActiveParents,
								   Pod2<-ActiveParents,
								   Pod1/=Pod2],						
   ok.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
start_infra_appls()->   
    {ok,StoppedApplInfoLists}=appl_server:stopped_appls(),    
    R_Nodelog=[create_infra_appl({PodNode,ApplSpec,App})||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
							  nodelog==App],
    R_db_etcd=[create_infra_appl({PodNode,ApplSpec,App})||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
							  db_etcd==App],
    R_infra_service=[create_infra_appl({PodNode,ApplSpec,App})||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
								infra_service==App],
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
create_infra_appl({PodNode,ApplSpec,nodelog})->
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
    
 
create_infra_appl({PodNode,ApplSpec,db_etcd}) ->
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
create_infra_appl({PodNode,ApplSpec,infra_service}) ->
    Result= case create_appl([{PodNode,ApplSpec,infra_service}]) of
		{error,Reason}->
		    {error,Reason};
		[{ok,PodNode,ApplSpec,infra_service}]->
		    case rpc:call(PodNode,infra_service,config,[],5000) of
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
    _=[{error,Reason}||{error,Reason}<-create_appl(StoppedUserApplications,[])],


    ok.

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
    io:format("Ping  ~p~n",[{rpc:call(PodNode,App,ping,[],2000),PodNode,ApplSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("Creat Appl Result ~p~n",[{Result,PodNode,ApplSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    create_appl(T,[{Result,PodNode,ApplSpec,App}|Acc]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
