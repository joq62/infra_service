%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(ops_pod_operator_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(HeartbeatTime,20*1000).

%% External exports
-export([
	 wanted_state/2,
	 get_pod/2,
	 
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
	       cluster_spec,
	       instance_id,
	       present_controller_nodes,
	       missing_controller_nodes,
	       present_worker_nodes,
	       missing_worker_nodes
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

	    
%% call
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


initiate(InstanceId)-> 
    gen_server:call(?MODULE, {initiate,InstanceId},infinity).
get_pod(ApplSpec,HostSpec)->
      gen_server:call(?MODULE, {get_pod,ApplSpec,HostSpec},infinity).

ping() ->
    gen_server:call(?MODULE, {ping}).
%% cast
heartbeat()-> 
    gen_server:cast(?MODULE, {heartbeat}).


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

    {ok, #state{cluster_spec=undefined,
		instance_id=undefined,
		present_controller_nodes=undefined,
		missing_controller_nodes=undefined,
		present_worker_nodes=undefined,
		missing_worker_nodes=undefined}}.   
 

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
handle_call({get_pod,ApplSpec,HostSpec},_From, State) ->
    % Candidates
    Reply=case rd:rpc_call(db_etcd,db_host_spec,read,[hostname,HostSpec],5000) of 
	      {error,Reason}->
		  {error,Reason};
	      {ok,HostName}->
		  case rd:rpc_call(db_etcd,db_appl_spec,read,[app,ApplSpec],5000) of
		      {error,Reason}->
			  {error,Reason};
		      {ok,App}->
			  Candidates=[PodNode||PodNode<-State#state.present_worker_nodes,
					       {ok,HostName}==rpc:call(PodNode,inet,gethostname,[],5000),
					       false==lists:keymember(App,1,rpc:call(PodNode,application,which_applications,[],5000))],
			  % lowest number of applications
			  NumApplCandidate=[{list_length:start(rpc:call(PodNode,application,which_applications,[],5000)),PodNode}||PodNode<-Candidates],
			  PrioritizedCandidates=[PodNode||{_,PodNode}<-lists:keysort(1,NumApplCandidate)],
			  case PrioritizedCandidates of
			      []->
				  [];
			      [Candidate|_] ->
				  {ok,Candidate}
			  end
		  end
	  end,
    {reply, Reply, State};

handle_call({ping},_From, State) ->
    Reply=pong,
    {reply, Reply, State};

handle_call({initiate,InstanceId},_From, State) ->
    AllEnvs=application:get_all_env(),
    {cluster_spec,ClusterSpec}=lists:keyfind(cluster_spec,1,AllEnvs),
     
    {ok,NumControllers}=rd:rpc_call(db_etcd,db_cluster_spec,read,
				     [num_controllers,ClusterSpec],5000),
    {ok,ControllerHostSpecs}=rd:rpc_call(db_etcd,db_cluster_spec,read,
					 [controller_host_specs,ClusterSpec],5000),
    ResultControllers=create_controller_pods(InstanceId,ClusterSpec,NumControllers,ControllerHostSpecs),
    io:format("INFO: ResultControllers ~p~n",[{ResultControllers, ?MODULE,?LINE}]),
    
    {ok,NumWorkers}=rd:rpc_call(db_etcd,db_cluster_spec,read,
				[num_workers,ClusterSpec],5000),
    {ok,WorkerHostSpecs}=rd:rpc_call(db_etcd,db_cluster_spec,read,
				     [worker_host_specs,ClusterSpec],5000),
    ResultWorkers=create_worker_pods(InstanceId,ClusterSpec,NumWorkers,WorkerHostSpecs),
    io:format("INFO: ResultControllers ~p~n",[{ResultWorkers, ?MODULE,?LINE}]),
    
    PresentControllerNodes=present_controller_nodes(InstanceId),
    MissingControllerNodes=missing_controller_nodes(InstanceId),
    io:format("INFO:PresentControllerNodes ~p~n",[PresentControllerNodes]),   
    io:format("INFO:MissingControllerNodes ~p~n",[MissingControllerNodes]),
    
    PresentWorkerNodes=present_worker_nodes(InstanceId),
    MissingWorkerNodes=missing_worker_nodes(InstanceId),
    io:format("INFO:PresentWorkerNodes ~p~n",[PresentWorkerNodes]),   
    io:format("INFO:MissingWorkerNodes ~p~n",[MissingWorkerNodes]),

    InitialState=State#state{cluster_spec=ClusterSpec,
			     instance_id=InstanceId,
			     present_controller_nodes=PresentControllerNodes,
			     missing_controller_nodes=MissingControllerNodes,
			     present_worker_nodes=PresentWorkerNodes,
			     missing_worker_nodes=MissingWorkerNodes
			    },
    rpc:cast(node(),?MODULE,heartbeat,[]),
    Reply=ok,
    {reply, Reply, InitialState};

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

handle_cast({heartbeat}, State) ->

    NewPresentControllerNodes=present_controller_nodes(State#state.instance_id),
    NewMissingControllerNodes=missing_controller_nodes(State#state.instance_id),
    NewPresentWorkerNodes=present_worker_nodes(State#state.instance_id),
    NewMissingWorkerNodes=missing_worker_nodes(State#state.instance_id),
  

    NoChangeStatusController=lists:sort(NewPresentControllerNodes) =:= lists:sort(State#state.present_controller_nodes),
    case NoChangeStatusController of
	false->
	    io:format("INFO: controller state changed  ~p~n",[{date(),time()}]),  
	   
	    io:format("INFO:PresentControllerNodes ~p~n",[State#state.present_controller_nodes]),   
	    io:format("INFO:MissingControllerNodes ~p~n",[State#state.missing_controller_nodes]),
	  
	    io:format("INFO:NewPresentControllerNodes ~p~n",[NewPresentControllerNodes]),   
	    io:format("INFO:NewMissingControllerNodes ~p~n",[NewMissingControllerNodes]);
	true->
	    ok
    end,
    NoChangeStatusWorker=lists:sort(NewPresentWorkerNodes)=:=lists:sort(State#state.present_worker_nodes),
    case NoChangeStatusWorker of
	false->
	    io:format("INFO: worker state changed  ~p~n",[{date(),time()}]),  
	    
	    io:format("INFO:PresentWorkerNodes ~p~n",[State#state.present_worker_nodes]),   
	    io:format("INFO:MissingWorkerNodes ~p~n",[State#state.missing_worker_nodes]),
	    
	    io:format("INFO:NewPresentWorkerNodes ~p~n",[NewPresentWorkerNodes]),   
	    io:format("INFO:NewMissingWorkerNodes ~p~n",[NewMissingWorkerNodes]);
	true->
	    ok
    end,

    NewState=State#state{present_controller_nodes=NewPresentControllerNodes,
			 missing_controller_nodes=NewMissingControllerNodes,
			 present_worker_nodes=NewPresentWorkerNodes,
			 missing_worker_nodes=NewMissingWorkerNodes},
  
    spawn(fun()->hbeat(State#state.instance_id,State#state.cluster_spec) end),
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
hbeat(InstanceId,ClusterSpec)->
    timer:sleep(?HeartbeatTime),
    rpc:call(node(),?MODULE,wanted_state,[InstanceId,ClusterSpec],30*1000), 
    rpc:cast(node(),?MODULE,heartbeat,[]).



%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
-define(TimeOut,10000).
    
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
wanted_state(InstanceId,_ClusterSpec)->
    MissingControllerNodes=missing_controller_nodes(InstanceId),
    MissingWorkerNodes=missing_worker_nodes(InstanceId),
    [restart_pod(InstanceId,PodNode)||PodNode<-MissingControllerNodes],
    [restart_pod(InstanceId,PodNode)||PodNode<-MissingWorkerNodes],
    ok.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
missing_controller_nodes(InstanceId)->
    [Node||Node<-db_cluster_instance:nodes(controller,InstanceId), 
	   pang=:=net_adm:ping(Node)].
present_controller_nodes(InstanceId)->
    [Node||Node<-db_cluster_instance:nodes(controller,InstanceId), 
	   pong=:=net_adm:ping(Node)].

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
missing_worker_nodes(InstanceId)->
    [Node||Node<-db_cluster_instance:nodes(worker,InstanceId), 
	   pang=:=net_adm:ping(Node)].
present_worker_nodes(InstanceId)->
    [Node||Node<-db_cluster_instance:nodes(worker,InstanceId), 
	   pong=:=net_adm:ping(Node)].
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
restart_pod(InstanceId,PodNode)->
    {ok,ClusterSpec}=rd:rpc_call(db_etcd,db_cluster_instance,read,[cluster_spec,InstanceId,PodNode],5000),
    {ok,HostSpec}=rd:rpc_call(db_etcd,db_cluster_instance,read,[host_spec,InstanceId,PodNode],5000),
    {ok,Cookie}=rd:rpc_call(db_etcd,db_cluster_spec,read,[cookie,ClusterSpec],5000),
    ConnectNodes=rd:rpc_call(db_etcd,db_cluster_instance,nodes,[connect,InstanceId],5000),
    
    {ok,HostName}=rd:rpc_call(db_etcd,db_host_spec,read,[hostname,HostSpec],5000),
  %  UniqueId=os:system_time(microsecond),
  %  PodName=erlang:integer_to_list(UniqueId,36)++"_"++ClusterSpec++"_controller",
    {ok,PodName}=rd:rpc_call(db_etcd,db_cluster_instance,read,[pod_name,InstanceId,PodNode],5000),
    rpc:call(PodNode,init,stop,[]),
    {ok,PodDir}=rd:rpc_call(db_etcd,db_cluster_instance,read,[pod_dir,InstanceId,PodNode],5000),
    
    PaArgs=" -detached ",
    EnvArgs=" ",
    io:format("INFO: restarts  pod ~p~n",[{PodNode, ?MODULE,?LINE}]),
    create_pod_node(HostName,PodName,PodDir,Cookie,PaArgs,EnvArgs,ConnectNodes,?TimeOut).


create_controller_pods(InstanceId,ClusterSpec,NumControllers,ControllerHostSpecs)->
    create_controller_pod(InstanceId,ClusterSpec,NumControllers,ControllerHostSpecs,[]).

create_controller_pod(_InstanceId,_ClusterSpec,0,_ControllerHostSpecs,Acc)->
    Acc;
create_controller_pod(InstanceId,ClusterSpec,N,[HostSpec|T],Acc) ->
    ConnectNodes=rd:rpc_call(db_etcd,db_cluster_instance,nodes,[connect,InstanceId],5000),
    {ok,Cookie}=rd:rpc_call(db_etcd,db_cluster_spec,read,[cookie,ClusterSpec],5000),
    {ok,ClusterDir}=rd:rpc_call(db_etcd,db_cluster_spec,read,[dir,ClusterSpec],5000),
    {ok,HostName}=rd:rpc_call(db_etcd,db_host_spec,read,[hostname,HostSpec],5000),
  %  UniqueId=os:system_time(microsecond),
  %  PodName=erlang:integer_to_list(UniqueId,36)++"_"++ClusterSpec++"_controller",
    PodName=integer_to_list(N)++"_"++ClusterSpec++"_controller",
    PodNode=list_to_atom(PodName++"@"++HostName),
    rpc:call(PodNode,init,stop,[]),
    PodDirName=PodName++".dir",
    PodDir=filename:join(ClusterDir,PodDirName),
    Type=controller,
    Status=candidate,
    db_cluster_instance:create(InstanceId,ClusterSpec,Type,PodName,PodNode,PodDir,HostSpec,Status),
    PaArgs=" -detached ",
    EnvArgs=" ",
    R=create_pod_node(HostName,PodName,PodDir,Cookie,PaArgs,EnvArgs,ConnectNodes,?TimeOut),
    RotatedHostSpecList=lists:append(T,[HostSpec]),
    io:format("INFO: Create controller pod ~p~n",[{PodNode, ?MODULE,?LINE}]),
    create_controller_pod(InstanceId,ClusterSpec,N-1,RotatedHostSpecList,[R|Acc]).
    



%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
create_worker_pods(InstanceId,ClusterSpec,NumWorkers,WorkerHostSpecs)->
    create_worker_pod(InstanceId,ClusterSpec,NumWorkers,WorkerHostSpecs,[]).

create_worker_pod(_InstanceId,_ClusterSpec,0,_WorkerHostSpecs,Acc)->
    Acc;
create_worker_pod(InstanceId,ClusterSpec,N,[HostSpec|T],Acc) ->
    ConnectNodes=rd:rpc_call(db_etcd,db_cluster_instance,nodes,[connect,InstanceId],5000),
    {ok,Cookie}=rd:rpc_call(db_etcd,db_cluster_spec,read,[cookie,ClusterSpec],5000),
    {ok,ClusterDir}=rd:rpc_call(db_etcd,db_cluster_spec,read,[dir,ClusterSpec],5000),
    {ok,HostName}=rd:rpc_call(db_etcd,db_host_spec,read,[hostname,HostSpec],5000),
   % UniqueId=os:system_time(microsecond),
  %  PodName=erlang:integer_to_list(UniqueId,36)++"_"++ClusterSpec++"_worker",
    PodName=integer_to_list(N)++"_"++ClusterSpec++"_worker",
    PodNode=list_to_atom(PodName++"@"++HostName),
    rpc:call(PodNode,init,stop,[]),
    PodDirName=PodName++".dir",
    PodDir=filename:join(ClusterDir,PodDirName),
    Type=worker,
    Status=candidate,
    db_cluster_instance:create(InstanceId,ClusterSpec,Type,PodName,PodNode,PodDir,HostSpec,Status),
    PaArgs=" -detached ",
    EnvArgs=" ",
    R=create_pod_node(HostName,PodName,PodDir,Cookie,PaArgs,EnvArgs,ConnectNodes,?TimeOut),
    RotatedHostSpecList=lists:append(T,[HostSpec]),
    io:format("INFO: Create worker pod ~p~n",[{PodNode, ?MODULE,?LINE}]),
    create_worker_pod(InstanceId,ClusterSpec,N-1,RotatedHostSpecList,[R|Acc]).
    
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
create_pod_node(HostName,PodName,PodDir,Cookie,PaArgs,EnvArgs,ConnectNodes,TimeOut)->
    case ops_vm:ssh_create(HostName,PodName,PodDir,Cookie,PaArgs,EnvArgs,ConnectNodes,TimeOut) of
	{error,Reason}->
	    {error,Reason};
	  {ok,PodNode,_,_}->
	     ApplSpec="pod_app",
	    {ok,PodApplGitPath}=rd:rpc_call(db_etcd,db_appl_spec,read,[gitpath,ApplSpec],5000),
	    ApplDir=filename:join([PodDir,ApplSpec]),
	    
	    ok=rpc:call(PodNode,file,make_dir,[ApplDir],5000),
	    {ok,_}=appl:git_clone_to_dir(PodNode,PodApplGitPath,ApplDir),
	    {ok,PodApp}=rd:rpc_call(db_etcd,db_appl_spec,read,[app,ApplSpec],5000),
	    ApplEbin=filename:join([ApplDir,"ebin"]),
	    Paths=[ApplEbin],
	    ok=appl:load(PodNode,PodApp,Paths),
	    ok=appl:start(PodNode,PodApp),
	    
	     % Init 
	    {ok,LocalTypeList}=rd:rpc_call(db_etcd,db_appl_spec,read,[local_type,ApplSpec],5000),
	    {ok,TargetTypeList}=rd:rpc_call(db_etcd,db_appl_spec,read,[target_type,ApplSpec],5000),
	    [rpc:call(PodNode,rd,add_local_resource,[LocalType,PodNode],5000)||LocalType<-LocalTypeList],
	    [rpc:call(PodNode,rd,add_target_resource_type,[TargetType],5000)||TargetType<-TargetTypeList],
	    rpc:call(PodNode,rd,trade_resources,[],5000),
	    timer:sleep(2000),
	    ok
    end.
