%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(pod_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([
	 create_controller_pods/2, 
	 create_worker_pods/2,
	 get_pod/2,
	 
	 ping/0
	]).


-export([
	 start_monitoring/2,
	 wanted_state/2,
	 heartbeat/0
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

create_controller_pods(ClusterSpec,InstanceId)->
    controller_pods(ClusterSpec,InstanceId).

create_worker_pods(ClusterSpec,InstanceId)->
    worker_pods(ClusterSpec,InstanceId).

get_pod(ApplSpec,HostSpec)->
      gen_server:call(?MODULE, {get_pod,ApplSpec,HostSpec},infinity).

ping() ->
    gen_server:call(?MODULE, {ping}).
%% cast
start_monitoring(ClusterSpec,InstanceId)->
    gen_server:cast(?MODULE, {start_monitoring,ClusterSpec,InstanceId}).

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
    Reply=case db_host_spec:read(hostname,HostSpec) of 
	      {error,Reason}->
		  {error,Reason};
	      {ok,HostName}->
		  case db_appl_spec:read(app,ApplSpec) of
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
handle_cast({start_monitoring,ClusterSpec,InstanceId}, State) ->

    PresentControllerNodes=present_controller_nodes(InstanceId),
    MissingControllerNodes=missing_controller_nodes(InstanceId),
    io:format("INFO:PresentControllerNodes ~p~n",[PresentControllerNodes]),   
    io:format("INFO:MissingControllerNodes ~p~n",[MissingControllerNodes]),
    
    PresentWorkerNodes=present_worker_nodes(InstanceId),
    MissingWorkerNodes=missing_worker_nodes(InstanceId),
    io:format("INFO:PresentWorkerNodes ~p~n",[PresentWorkerNodes]),   
    io:format("INFO:MissingWorkerNodes ~p~n",[MissingWorkerNodes]),

    NewState=State#state{cluster_spec=ClusterSpec,
			     instance_id=InstanceId,
			     present_controller_nodes=PresentControllerNodes,
			     missing_controller_nodes=MissingControllerNodes,
			     present_worker_nodes=PresentWorkerNodes,
			     missing_worker_nodes=MissingWorkerNodes
			    },

    spawn(fun()->hbeat(ClusterSpec,InstanceId) end),
    {noreply, NewState};

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
  
    spawn(fun()->hbeat(State#state.cluster_spec,State#state.instance_id) end),
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
hbeat(InstanceId,ClusterSpec)->
    rpc:call(node(),?MODULE,wanted_state,[ClusterSpec,InstanceId],30*1000), 
    rpc:cast(node(),?MODULE,heartbeat,[]).

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
-define(TimeOut,10*1000).

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
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
restart_pod(InstanceId,PodNode)->
    {ok,ClusterSpec}=db_cluster_instance:read(cluster_spec,InstanceId,PodNode),
    {ok,HostSpec}=db_cluster_instance:read(host_spec,InstanceId,PodNode),
    {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
    ConnectNodes=db_cluster_instance:nodes(connect,InstanceId),
    
    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
  %  UniqueId=os:system_time(microsecond),
  %  PodName=erlang:integer_to_list(UniqueId,36)++"_"++ClusterSpec++"_controller",
    {ok,PodName}=db_cluster_instance:read(pod_name,InstanceId,PodNode),
    rpc:call(PodNode,init,stop,[]),
    {ok,PodDir}=db_cluster_instance:read(pod_dir,InstanceId,PodNode),
    
    PaArgs=" -detached ",
    EnvArgs=" ",
    io:format("INFO: restarts  pod ~p~n",[{PodNode, ?MODULE,?LINE}]),
    create_pod_node(HostName,PodName,PodDir,Cookie,PaArgs,EnvArgs,ConnectNodes,?TimeOut).


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
controller_pods(ClusterSpec,InstanceId)->
    {ok,NumControllers}=db_cluster_spec:read(num_controllers,ClusterSpec),
    {ok,ControllerHostSpecs}=db_cluster_spec:read(controller_host_specs,ClusterSpec),
    create_controller_pod(InstanceId,ClusterSpec,NumControllers,ControllerHostSpecs,[]).
    
create_controller_pod(_InstanceId,_ClusterSpec,0,_ControllerHostSpecs,Acc)->
    Acc;
create_controller_pod(InstanceId,ClusterSpec,N,[HostSpec|T],Acc) ->
    ConnectNodes=db_cluster_instance:nodes(connect,InstanceId),
    {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
    {ok,ClusterDir}=db_cluster_spec:read(dir,ClusterSpec),
    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
    
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
worker_pods(ClusterSpec,InstanceId)->
    {ok,NumWorkers}=db_cluster_spec:read(num_workers,ClusterSpec),
    {ok,WorkerHostSpecs}=db_cluster_spec:read(worker_host_specs,ClusterSpec),
    create_worker_pod(InstanceId,ClusterSpec,NumWorkers,WorkerHostSpecs,[]).

create_worker_pod(_InstanceId,_ClusterSpec,0,_WorkerHostSpecs,Acc)->
    Acc;
create_worker_pod(InstanceId,ClusterSpec,N,[HostSpec|T],Acc) ->
    ConnectNodes=db_cluster_instance:nodes(connect,InstanceId),
    {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
    {ok,ClusterDir}=db_cluster_spec:read(dir,ClusterSpec),
    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
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
	    ok
    end.
