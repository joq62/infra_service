%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(appl_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(HeartbeatTime,20*1000).
-define(TimeOut,10*1000).

%% External exports
-export([
	 deploy_appls/1,
	 new/4,
	 delete/3,
	 present_apps/1,
	 missing_apps/1,
	 
	 initiate/1,
	 start_monitoring/1,
	 heartbeat/0,
	 wanted_state/1,
	 restart_appl/2,
	 ping/0
	]).


-export([
	 start/0,
	 stop/0
	]).


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------
-record(state,{
	      cluster_spec,
	       present,
	       missing
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

	    
%% call
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

new(ApplSpec,HostSpec,ClusterSpec,TimeOut)->
    gen_server:call(?MODULE, {new,ApplSpec,HostSpec,ClusterSpec,TimeOut},infinity).

delete(ApplSpec,PodNode,ClusterSpec)->
    gen_server:call(?MODULE, {delete,ApplSpec,PodNode,ClusterSpec},infinity).

deploy_appls(ClusterSpec)->
    gen_server:call(?MODULE, {deploy_appls,ClusterSpec},infinity).

present_apps(ClusterSpec) ->
    gen_server:call(?MODULE, {present_apps,ClusterSpec}).

missing_apps(ClusterSpec) ->
    gen_server:call(?MODULE, {missing_apps,ClusterSpec}).

ping() ->
    gen_server:call(?MODULE, {ping}).

%% cast
start_monitoring(ClusterSpec)->
    gen_server:cast(?MODULE, {start_monitoring,ClusterSpec}).

heartbeat()-> 
    gen_server:cast(?MODULE, {heartbeat}).
initiate(InstanceId)-> 
    gen_server:call(?MODULE, {initiate,InstanceId},infinity).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([]) -> 

    
    io:format("Started Server ~p~n",[{?MODULE,?LINE}]),
    rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,"Servere started"]),
    {ok, #state{
	    cluster_spec=undefined,
	    present=undefined,
	    missing=undefined}}.   
 

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_call({new,ApplSpec,HostSpec,ClusterSpec,TimeOut},_From, State) ->
    Reply=appl_new(ApplSpec,HostSpec,ClusterSpec,TimeOut),
    {reply, Reply, State};

handle_call({delete,ApplSpec,PodNode,ClusterSpec},_From, State) ->
    Reply=appl_del(ApplSpec,PodNode,ClusterSpec),
    {reply, Reply, State};

handle_call({deploy_appls,ClusterSpec},_From, State) ->
    Reply=deploy(ClusterSpec),
      {reply, Reply, State};

handle_call({present_apps,ClusterSpec},_From, State) ->
    Reply=present(ClusterSpec),
    {reply, Reply, State};

handle_call({missing_apps,ClusterSpec},_From, State) ->
    Reply=missing(ClusterSpec),
    {reply, Reply, State};

handle_call({initiate,ClusterSpec},_From, State) ->

    Reply=ok,
    {reply, Reply, State#state{cluster_spec=ClusterSpec}};

handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call(Request, From, State) ->
    Reply = {unmatched_signal,?MODULE,Request,From},
    {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({start_monitoring,ClusterSpec}, State) ->
    Present=present(ClusterSpec),
    Missing=missing(ClusterSpec),
    io:format("INFO:Present ~p~n",[Present]),   
    io:format("INFO:Missing ~p~n",[Missing]),
    NewState=State#state{cluster_spec=ClusterSpec,
			 present=Present,
			 missing=Missing		
			},

    spawn(fun()->hbeat(ClusterSpec) end),
    {noreply, NewState};

handle_cast({heartbeat}, State) ->

    NewPresent=present(State#state.cluster_spec),
    NewMissing=missing(State#state.cluster_spec),
    NoChangeStatusController=lists:sort(NewPresent) =:= lists:sort(State#state.present),
    case NoChangeStatusController of
	false->
	    io:format("INFO: state changed  ~p~n",[{date(),time()}]),  
	   
	    io:format("INFO:Present ~p~n",[State#state.present]),   
	    io:format("INFO:Missing ~p~n",[State#state.missing]),
	  
	    io:format("INFO:NewPresent ~p~n",[NewPresent]),   
	    io:format("INFO:NewMissing ~p~n",[NewMissing]);
	true->
	    ok
    end,
    NewState=State#state{present=NewPresent,
			 missing=NewMissing},
  
    spawn(fun()->hbeat(State#state.cluster_spec) end),
    {noreply, NewState};


handle_cast(Msg, State) ->
    io:format("unmatched match cast ~p~n",[{Msg,?MODULE,?LINE}]),
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info({ssh_cm,_,{closed,0}}, State) ->
    {noreply, State};

handle_info(Info, State) ->
    io:format("unmatched match~p~n",[{Info,?MODULE,?LINE}]), 
    {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
appl_del(ApplSpec,PodNode,ClusterSpec)->
    Result=case rd:rpc_call(db_etcd,db_cluster_instance,read,[pod_dir,ClusterSpec,PodNode],5000) of
		    {error,Reason}->
			{error,Reason};
		    {ok,PodDir}->
		   case rd:rpc_call(db_etcd,db_appl_spec,read,[app,ApplSpec],5000) of
		       {error,Reason}->
			   {error,Reason};	       
		       {ok,PodApp}->
			   case appl:stop(PodNode,PodApp) of
			       {error,Reason}->
				   {error,Reason};
			       ok->
				   ApplDir=filename:join([PodDir,ApplSpec]),
				   case appl:unload(PodNode,PodApp,ApplDir) of
				       {error,Reason}->
					   {error,Reason};
				       ok->
					   case rd:rpc_call(db_etcd,db_appl_spec,read,[local_type,ApplSpec],5000) of
					       {error,Reason}->
						   {error,Reason}; 
					       {ok,LocalTypeList}->
						   [rpc:call(PodNode,rd,delete_local_resource,[LocalType,PodNode],5000)||LocalType<-LocalTypeList],
						   rpc:call(PodNode,rd,trade_resources,[],5000),
						   timer:sleep(2000),
						   case rd:rpc_call(db_etcd,db_appl_instance,delete,[ClusterSpec,ApplSpec,PodNode],5000) of
						       {atomic,ok}->
							   ok;
						       Reason ->
							   {error,Reason,?MODULE,?LINE}
						   end
					   end
				   end
			   end
		   end
	   end,
    Result.
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
appl_new(ApplSpec,HostSpec,ClusterSpec,TimeOut)->
    Result=case pod_server:get_pod(ApplSpec,HostSpec) of
	       {error,Reason}->
		   rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,{error,Reason}]),
		   {error,Reason};
	       []->
		   rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,{error,[no_pods_available]}]),
		   {error,[no_pods_available,?MODULE,?LINE]};
	       {ok,PodNode}->
		   {ok,PodDir}=db_cluster_instance:read(pod_dir,ClusterSpec,PodNode),
		   {ok,PodApplGitPath}=db_appl_spec:read(gitpath,ApplSpec),
		   ApplDir=filename:join([PodDir,ApplSpec]),
		   %% set application envs
		   {ok,ApplicationConfig}=db_host_spec:read(application_config,HostSpec),  
		   _SetEnvResult=[rpc:call(PodNode,application,set_env,[[Config]],5000)||Config<-ApplicationConfig],
		   %io:format("DEBUG: SetEnvResult ~p~n",[SetEnvResult]),

		   ok=rpc:call(PodNode,file,make_dir,[ApplDir],5000),
		   {ok,_}=appl:git_clone_to_dir(PodNode,PodApplGitPath,ApplDir),
		   {ok,PodApp}=db_appl_spec:read(app,ApplSpec),
		   ApplEbin=filename:join([ApplDir,"ebin"]),
		   Paths=[ApplEbin],
		   
		   ok=appl:load(PodNode,PodApp,Paths),
		   ok=appl:start(PodNode,PodApp,TimeOut),
		 
	    
	     % Init 
		   {ok,LocalTypeList}=db_appl_spec:read(local_type,ApplSpec),
		   [rpc:call(PodNode,rd,add_local_resource,[LocalType,PodNode],5000)||LocalType<-LocalTypeList],
		   %% Make it available for oam - debug 
		   [rd:add_target_resource_type(LocalType)||LocalType<-LocalTypeList],

		   {ok,TargetTypeList}=db_appl_spec:read(target_type,ApplSpec),
		   [rpc:call(PodNode,rd,add_target_resource_type,[TargetType],5000)||TargetType<-TargetTypeList],
		   
		   rpc:call(PodNode,rd,trade_resources,[],5000),
		   timer:sleep(2000),
		   {atomic,ok}=db_appl_instance:create(ClusterSpec,ApplSpec,PodNode,HostSpec,{date(),time()}),
		   rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["application created ",ApplSpec,PodNode]]),
		   {ok,PodNode}
	   end,
    Result.
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
present(ClusterSpec)->
    AppPodList=[{db_appl_spec:read(app,AppSpec),PodNode,AppSpec}||{AppSpec,PodNode}<-db_appl_instance:get_pod_appl_specs(ClusterSpec)],
    [{AppSpec,PodNode,App}||{{ok,App},PodNode,AppSpec}<-AppPodList, 
			 true==rpc:call(node(),lists,keymember,[App,1,rpc:call(PodNode,application,which_applications,[],2000)])].
missing(ClusterSpec)->
    AppPodList=[{db_appl_spec:read(app,AppSpec),PodNode,AppSpec}||{AppSpec,PodNode}<-db_appl_instance:get_pod_appl_specs(ClusterSpec)],
    [{AppSpec,PodNode}||{{ok,App},PodNode,AppSpec}<-AppPodList, 
			true/=rpc:call(node(),lists,keymember,[App,1,rpc:call(PodNode,application,which_applications,[],2000)])].


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
hbeat(ClusterSpec)->
    timer:sleep(?HeartbeatTime),
    rpc:call(node(),?MODULE,wanted_state,[ClusterSpec],30*1000), 
    rpc:cast(node(),?MODULE,heartbeat,[]).
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
wanted_state(ClusterSpec)->
      % {AppSpec,PodNode}
    
    MissingPresentNodes=[{ApplSpec,PodNode}||{ApplSpec,PodNode}<-missing(ClusterSpec),
					     pong==net_adm:ping(PodNode)],
    [rpc:call(node(),?MODULE,restart_appl,[ClusterSpec,{ApplSpec,PodNode}],10*1000)||{ApplSpec,PodNode}<-MissingPresentNodes],
 %   io:format(" ~p~n",[{R,?MODULE,?FUNCTION_NAME}]),
    ok.
    
    
 %% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
restart_appl(ClusterSpec,{ApplSpec,PodNode})->
    {ok,HostSpec}=db_appl_instance:read(host_spec,ClusterSpec,PodNode),
    {ok,PodDir}=db_cluster_instance:read(pod_dir,ClusterSpec,PodNode),
    {ok,PodApplGitPath}=db_appl_spec:read(gitpath,ApplSpec),
    ApplDir=filename:join([PodDir,ApplSpec]),
		   %% set application envs
    {ok,ApplicationConfig}=db_host_spec:read(application_config,HostSpec),  
    _SetEnvResult=[rpc:call(PodNode,application,set_env,[[Config]],5000)||Config<-ApplicationConfig],
		   %io:format("DEBUG: SetEnvResult ~p~n",[SetEnvResult]),

    {ok,PodApp}=db_appl_spec:read(app,ApplSpec),
    appl:stop(PodNode,PodApp),
    appl:unload(PodNode,PodApp,ApplDir),
    ok=rpc:call(PodNode,file,make_dir,[ApplDir],5000),
    {ok,_}=appl:git_clone_to_dir(PodNode,PodApplGitPath,ApplDir),
  
    ApplEbin=filename:join([ApplDir,"ebin"]),
    Paths=[ApplEbin],
   
    ok=appl:load(PodNode,PodApp,Paths),
    ok=appl:start(PodNode,PodApp,?TimeOut),
    ok.
    
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
deploy(ClusterSpec)->
    ApplDeploySpecList=db_appl_deployment:read_all(),
    io:format(" ApplDeploySpecList ~p~n",[{ApplDeploySpecList,?MODULE,?LINE}]),
    start_appl(ApplDeploySpecList,ClusterSpec).

% Affinity: any_host,[HostSpec,,,HostSpecN]
% db_appl_deployment:create(SpecId,ApplSpec,Vsn,ClusterSpec,NumInstances,Affinity)
% {SpecId,ApplSpec,Vsn,ClusterSpec,NumInstances,Affinity}

 % pod_server:get_pod(ApplSpec,HostSpec)
% pod_server:new(ApplSpec,HostSpec,ClusterSpec,InstanceId)
% db_appl_instance:create(ClusterSpec,ClusterInstance,ApplSpec,PodNode,HostSpec,Status)

% {ok,PodNode} appl_server:new(ApplSpec,HostSpec,ClusterSpec,InstanceId)

% {appl_deployment,"math",
% [{appl_spec,"math"},
%  {vsn,"0.1.0"},	
%  {cluster_spec,"c200_c201"},
%  {num_instances,2 },
%  {affinity,any_host}	
% ]
% }.
start_appl([],_ClusterSpec)->
    ok;
          
start_appl([{_Id,ApplSpec,_Vsn,ClusterSpec,NumInstances,Affinity}|T],CurrentClusterSpec)->
    case (CurrentClusterSpec == ClusterSpec) of
	false->
	    false;
	true->
	    {ok,WorkerHostSpecs}=db_cluster_spec:read(worker_host_specs,ClusterSpec),
	    start_appl(ApplSpec,ClusterSpec,NumInstances,Affinity,WorkerHostSpecs)
    end,
    start_appl(T,ClusterSpec).

start_appl(_ApplSpec,_ClusterSpec,0,_Affinity,_WorkerHostSpecs)->
    ok;
start_appl(ApplSpec,ClusterSpec,N,any_host,[HostSpec|T])->
    _R=appl_new(ApplSpec,HostSpec,ClusterSpec,60*1000),
    RotatedHostSpecList=lists:append(T,[HostSpec]),
    start_appl(ApplSpec,ClusterSpec,N-1,any_host,RotatedHostSpecList);
start_appl(ApplSpec,ClusterSpec,N,[HostSpec|T],WorkerHostSpecs)->
    _R=appl_new(ApplSpec,HostSpec,ClusterSpec,60*1000),
    RotatedHostSpecList=lists:append(T,[HostSpec]),
    start_appl(ApplSpec,ClusterSpec,N-1,RotatedHostSpecList,WorkerHostSpecs).

	    
