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

%% External exports
-export([
	 deploy_appls/1,
	 new/3,
	 delete/3,
	 present_apps/1,
	 missing_apps/1,
	 
	 initiate/1,
	 heartbeat/0,
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
	      cluster_spec
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

	    
%% call
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

new(ApplSpec,HostSpec,ClusterSpec)->
    gen_server:call(?MODULE, {new,ApplSpec,HostSpec,ClusterSpec},infinity).

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

    {ok, #state{
	        cluster_spec=undefined}}.   
 

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
handle_call({new,ApplSpec,HostSpec,ClusterSpec},_From, State) ->
    Reply=appl_new(ApplSpec,HostSpec,ClusterSpec),
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
appl_new(ApplSpec,HostSpec,ClusterSpec)->
    Result=case pod_server:get_pod(ApplSpec,HostSpec) of
	       {error,Reason}->
		   {error,Reason};
	       []->
		   {error,[no_pods_available,?MODULE,?LINE]};
	       {ok,PodNode}->
		   {ok,PodDir}=db_cluster_instance:read(pod_dir,ClusterSpec,PodNode),
		   {ok,PodApplGitPath}=db_appl_spec:read(gitpath,ApplSpec),
		   ApplDir=filename:join([PodDir,ApplSpec]),
	    
		   ok=rpc:call(PodNode,file,make_dir,[ApplDir],5000),
		   {ok,_}=appl:git_clone_to_dir(PodNode,PodApplGitPath,ApplDir),
		   {ok,PodApp}=db_appl_spec:read(app,ApplSpec),
		   ApplEbin=filename:join([ApplDir,"ebin"]),
		   Paths=[ApplEbin],
		   ok=appl:load(PodNode,PodApp,Paths),
		   ok=appl:start(PodNode,PodApp),
	    
	     % Init 
		   {ok,LocalTypeList}=db_appl_spec:read(local_type,ApplSpec),
		   {ok,TargetTypeList}=db_appl_spec:read(target_type,ApplSpec),
		   [rpc:call(PodNode,rd,add_local_resource,[LocalType,PodNode],5000)||LocalType<-LocalTypeList],
		   [rpc:call(PodNode,rd,add_target_resource_type,[TargetType],5000)||TargetType<-TargetTypeList],
		   rpc:call(PodNode,rd,trade_resources,[],5000),
		   timer:sleep(2000),
		   {atomic,ok}=db_appl_instance:create(ClusterSpec,ApplSpec,PodNode,HostSpec,{date(),time()}),
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
    [{AppSpec,PodNode}||{{ok,App},PodNode,AppSpec}<-AppPodList, 
			 pong==rpc:call(PodNode,App,ping,[],2000)].
missing(ClusterSpec)->
    AppPodList=[{db_appl_spec:read(app,AppSpec),PodNode,AppSpec}||{AppSpec,PodNode}<-db_appl_instance:get_pod_appl_specs(ClusterSpec)],
    [{AppSpec,PodNode}||{{ok,App},PodNode,AppSpec}<-AppPodList, 
			 pong/=rpc:call(PodNode,App,ping,[],2000)].

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
    _R=appl_new(ApplSpec,HostSpec,ClusterSpec),
    RotatedHostSpecList=lists:append(T,[HostSpec]),
    start_appl(ApplSpec,ClusterSpec,N-1,any_host,RotatedHostSpecList);
start_appl(ApplSpec,ClusterSpec,N,[HostSpec|T],WorkerHostSpecs)->
    _R=appl_new(ApplSpec,HostSpec,ClusterSpec),
    RotatedHostSpecList=lists:append(T,[HostSpec]),
    start_appl(ApplSpec,ClusterSpec,N-1,RotatedHostSpecList,WorkerHostSpecs).

	    
