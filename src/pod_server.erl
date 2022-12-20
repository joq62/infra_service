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
-define(HeartbeatTime,20*1000).

%% --------------------------------------------------------------------

%% External exports
-export([
	 create_controller_pods/1, 
	 create_worker_pods/1,
	 get_pod/2,
	 present_controller_nodes/0,
	 present_worker_nodes/0,
	 
	 ping/0
	]).


-export([
	 start_monitoring/1,
	 wanted_state/1,
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

create_controller_pods(ClusterSpec)->
    gen_server:call(?MODULE, {create_controller_pods,ClusterSpec},infinity).

create_worker_pods(ClusterSpec)->
    gen_server:call(?MODULE, {create_worker_pods,ClusterSpec},infinity).

get_pod(ApplSpec,HostSpec)->
      gen_server:call(?MODULE, {get_pod,ApplSpec,HostSpec},infinity).

present_controller_nodes()->
    gen_server:call(?MODULE, {present_controllers},infinity).
present_worker_nodes()->
    gen_server:call(?MODULE, {present_workers},infinity).

ping() ->
    gen_server:call(?MODULE, {ping}).
%% cast
start_monitoring(ClusterSpec)->
    gen_server:cast(?MODULE, {start_monitoring,ClusterSpec}).

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
    rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Servere started"]]),
    {ok, #state{cluster_spec=undefined,
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

handle_call({create_controller_pods,ClusterSpec},_From, State) ->
    controller_pods(ClusterSpec),
    PresentControllerNodes=present_controller_nodes(ClusterSpec),
    MissingControllerNodes=missing_controller_nodes(ClusterSpec),
    NewState=State#state{present_controller_nodes=PresentControllerNodes,
			 missing_controller_nodes=MissingControllerNodes		    
			},
    Reply={PresentControllerNodes,MissingControllerNodes},
    {reply, Reply, NewState};

handle_call({create_worker_pods,ClusterSpec},_From, State) ->
    worker_pods(ClusterSpec,ClusterSpec) ,
    PresentWorkerNodes=present_worker_nodes(ClusterSpec),
    MissingWorkerNodes=missing_worker_nodes(ClusterSpec),
    NewState=State#state{present_worker_nodes=PresentWorkerNodes,
			 missing_worker_nodes=MissingWorkerNodes		    
			},
    Reply={PresentWorkerNodes,MissingWorkerNodes},
    {reply, Reply, NewState};

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


handle_call({present_controllers},_From, State) ->
    Reply=State#state.present_controller_nodes,
    {reply, Reply, State};
handle_call({present_workers},_From, State) ->
    Reply=State#state.present_worker_nodes,
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
handle_cast({start_monitoring,ClusterSpec}, State) ->

    PresentControllerNodes=present_controller_nodes(ClusterSpec),
    MissingControllerNodes=missing_controller_nodes(ClusterSpec),
       
    PresentWorkerNodes=present_worker_nodes(ClusterSpec),
    MissingWorkerNodes=missing_worker_nodes(ClusterSpec),
  

    NewState=State#state{cluster_spec=ClusterSpec,
			 present_controller_nodes=PresentControllerNodes,
			 missing_controller_nodes=MissingControllerNodes,
			 present_worker_nodes=PresentWorkerNodes,
			 missing_worker_nodes=MissingWorkerNodes
			},

    spawn(fun()->hbeat(ClusterSpec) end),
    {noreply, NewState};

handle_cast({heartbeat}, State) ->

    NewPresentControllerNodes=present_controller_nodes(State#state.cluster_spec),
    StartedControllers=[Node||Node<-NewPresentControllerNodes,
			      false==lists:member(Node,State#state.present_controller_nodes)],
    case StartedControllers of
	[]->
	    no_change;
	StartedControllers ->
	    rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Started Controller nodes : ",StartedControllers]])
    end,
    
    NewMissingControllerNodes=missing_controller_nodes(State#state.cluster_spec),
    StoppedControllers=[Node||Node<-NewMissingControllerNodes,
			      false==lists:member(Node,State#state.missing_controller_nodes)],
    case StoppedControllers of
	[]->
	    no_change;
	StoppedControllers ->
	    rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Stopped Controller nodes : ",StoppedControllers]])
    end,

    NewPresentWorkerNodes=present_worker_nodes(State#state.cluster_spec),
    StartedWorkers=[Node||Node<-NewPresentWorkerNodes,
			  false==lists:member(Node,State#state.present_worker_nodes)],
    case StartedWorkers of
	[]->
	    no_change;
	StartedWorkers ->
	    rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Started Worker nodes : ",StartedWorkers]])
    end,
    
    NewMissingWorkerNodes=missing_worker_nodes(State#state.cluster_spec),
    StoppedWorkers=[Node||Node<-NewMissingControllerNodes,
			  false==lists:member(Node,State#state.missing_controller_nodes)],
    case StoppedWorkers of
	[]->
	    no_change;
	StoppedWorkers ->
	    rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Stopped worker nodes : ",StoppedWorkers]])
    end,


    NewState=State#state{present_controller_nodes=NewPresentControllerNodes,
			 missing_controller_nodes=NewMissingControllerNodes,
			 present_worker_nodes=NewPresentWorkerNodes,
			 missing_worker_nodes=NewMissingWorkerNodes},
  
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


handle_info({ssh_cm,_,_}, State) ->
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
hbeat(ClusterSpec)->
    timer:sleep(?HeartbeatTime),
    rpc:call(node(),?MODULE,wanted_state,[ClusterSpec],30*1000), 
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
wanted_state(ClusterSpec)->
    MissingControllerNodes=missing_controller_nodes(ClusterSpec),
    MissingWorkerNodes=missing_worker_nodes(ClusterSpec),
    [restart_pod(ClusterSpec,PodNode)||PodNode<-MissingControllerNodes],
    [restart_pod(ClusterSpec,PodNode)||PodNode<-MissingWorkerNodes],
    ok.
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
missing_controller_nodes(ClusterSpec)->
    [Node||Node<-db_cluster_instance:nodes(controller,ClusterSpec), 
	   pang=:=net_adm:ping(Node)].
present_controller_nodes(ClusterSpec)->
    [Node||Node<-db_cluster_instance:nodes(controller,ClusterSpec), 
	   pong=:=net_adm:ping(Node)].

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
missing_worker_nodes(ClusterSpec)->
    [Node||Node<-db_cluster_instance:nodes(worker,ClusterSpec), 
	   pang=:=net_adm:ping(Node)].
present_worker_nodes(ClusterSpec)->
    [Node||Node<-db_cluster_instance:nodes(worker,ClusterSpec), 
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
restart_pod(ClusterSpec,PodNode)->
    {ok,HostSpec}=db_cluster_instance:read(host_spec,ClusterSpec,PodNode),
    {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
    ConnectNodes=db_cluster_instance:nodes(connect,ClusterSpec),
    
    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
  %  UniqueId=os:system_time(microsecond),
  %  PodName=erlang:integer_to_list(UniqueId,36)++"_"++ClusterSpec++"_controller",
    {ok,PodName}=db_cluster_instance:read(pod_name,ClusterSpec,PodNode),
    rpc:call(PodNode,init,stop,[]),
    {ok,PodDir}=db_cluster_instance:read(pod_dir,ClusterSpec,PodNode),
    
    PaArgs=" -detached ",
    EnvArgs=" ",
    create_pod_node(HostName,PodName,PodDir,Cookie,PaArgs,EnvArgs,ConnectNodes,?TimeOut).


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
controller_pods(ClusterSpec)->
    {ok,NumControllers}=db_cluster_spec:read(num_controllers,ClusterSpec),
    {ok,ControllerHostSpecs}=db_cluster_spec:read(controller_host_specs,ClusterSpec),
    create_controller_pod(ClusterSpec,NumControllers,ControllerHostSpecs,[]).
    
create_controller_pod(_ClusterSpec,0,_ControllerHostSpecs,Acc)->
    Acc;
create_controller_pod(ClusterSpec,N,[HostSpec|T],Acc) ->
    ConnectNodes=db_cluster_instance:nodes(connect,ClusterSpec),
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
    db_cluster_instance:create(ClusterSpec,Type,PodName,PodNode,PodDir,HostSpec,Status),
    PaArgs=" -detached ",
    EnvArgs=" ",
    Result=case create_pod_node(HostName,PodName,PodDir,Cookie,PaArgs,EnvArgs,ConnectNodes,?TimeOut) of
	       {error,Reason}->
		   rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Failed to create ",PodNode ,Reason]]),
		   {error,Reason};
	       {ok,PodNode,NodeDir,PingResult}->
		   rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Controller created ",PodNode]]),
		   {ok,PodNode,NodeDir,PingResult} 
	   end,
    RotatedHostSpecList=lists:append(T,[HostSpec]),
    create_controller_pod(ClusterSpec,N-1,RotatedHostSpecList,[Result|Acc]).
    
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
worker_pods(ClusterSpec,ClusterSpec)->
    {ok,NumWorkers}=db_cluster_spec:read(num_workers,ClusterSpec),
    {ok,WorkerHostSpecs}=db_cluster_spec:read(worker_host_specs,ClusterSpec),
    create_worker_pod(ClusterSpec,NumWorkers,WorkerHostSpecs,[]).

create_worker_pod(_ClusterSpec,0,_WorkerHostSpecs,Acc)->
    Acc;
create_worker_pod(ClusterSpec,N,[HostSpec|T],Acc) ->
    ConnectNodes=db_cluster_instance:nodes(connect,ClusterSpec),
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
    db_cluster_instance:create(ClusterSpec,Type,PodName,PodNode,PodDir,HostSpec,Status),
    PaArgs=" -detached ",
    EnvArgs=" ",
    Result=case create_pod_node(HostName,PodName,PodDir,Cookie,PaArgs,EnvArgs,ConnectNodes,?TimeOut) of
	       {error,Reason}->
		   rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Failed to create ",PodNode ,Reason]]),
		   {error,Reason};
	       {ok,PodNode,NodeDir,PingResult}->
		   rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Worker created ",PodNode]]),
		   {ok,PodNode,NodeDir,PingResult} 
	   end,
    RotatedHostSpecList=lists:append(T,[HostSpec]),
    create_worker_pod(ClusterSpec,N-1,RotatedHostSpecList,[Result|Acc]).
    
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
create_pod_node(HostName,PodName,PodDir,Cookie,PaArgs,EnvArgs,ConnectNodes,TimeOut)->
    case ops_vm:ssh_create(HostName,PodName,PodDir,Cookie,PaArgs,EnvArgs,ConnectNodes,TimeOut) of
	{error,Reason}->
	    {error,Reason};
	  {ok,PodNode,NodeDir,PingResult}->
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
	    {ok,PodNode,NodeDir,PingResult}
    end.
