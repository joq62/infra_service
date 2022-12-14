%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(connect_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(HeartbeatTime,20*1000).

 

%% External exports
-export([
	 create_dbase_info/2,
	 create_connect_nodes/2
	]).


-export([
	 start_monitoring/2,
	 wanted_state/2,
	 heartbeat/0
	]).
-export([
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
	       present_connect_nodes,
	       missing_connect_nodes
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

	    
%% call
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

create_dbase_info(ClusterSpec,InstanceId)->
    gen_server:call(?MODULE,{create_dbase_info,ClusterSpec,InstanceId},infinity).

create_connect_nodes(ClusterSpec,InstanceId)->
    gen_server:call(?MODULE,{create_connect_nodes,ClusterSpec,InstanceId},infinity).

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
		missing_connect_nodes=undefined,
		present_connect_nodes=undefined}}.   
 

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
handle_call({create_dbase_info,ClusterSpec,InstanceId},_From, State) ->
    Reply=dbase_info(ClusterSpec,InstanceId),
    {reply, Reply, State};

handle_call({create_connect_nodes,ClusterSpec,InstanceId},_From, State) ->
    Reply=connect_nodes(ClusterSpec,InstanceId),
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
    NewState=State#state{
	       cluster_spec=ClusterSpec,
	       instance_id=InstanceId,
	       present_connect_nodes=[],
	       missing_connect_nodes=[]
	      },
    spawn(fun()->hbeat(ClusterSpec,InstanceId) end),
    {noreply, NewState};


handle_cast({heartbeat}, State) ->
    NewPresentConnectNodes=present_connect_nodes(State#state.instance_id),
    NewMissingConnectNodes=missing_connect_nodes(State#state.instance_id),
   
    
    NoChangeStatus=lists:sort(NewPresentConnectNodes) =:= lists:sort(State#state.present_connect_nodes),
    case NoChangeStatus of
	false->
	    io:format("INFO: cluster state changed  ~p~n",[{date(),time()}]),  
	   
	    io:format("INFO:PresentConnectNodes ~p~n",[State#state.present_connect_nodes]),   
	    io:format("INFO:MissingConnectNodes ~p~n",[State#state.missing_connect_nodes]),
	  
	    io:format("INFO:NewPresentConnectNodes ~p~n",[NewPresentConnectNodes]),   
	    io:format("INFO:NewMissingConnectNodes ~p~n",[NewMissingConnectNodes]);
	true->
	    ok
    end,

    NewState=State#state{present_connect_nodes=NewPresentConnectNodes,
			 missing_connect_nodes=NewMissingConnectNodes},
  
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
hbeat(InstanceId,ClusterSpec)->
    rpc:call(node(),?MODULE,wanted_state,[ClusterSpec,InstanceId],30*1000), 
    rpc:cast(node(),?MODULE,heartbeat,[]).
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
wanted_state(ClusterSpec,InstanceId)->
    NodesToConnect=db_cluster_instance:nodes(connect,InstanceId),
    MissingConnectNodes=missing_connect_nodes(InstanceId),
    [create_connect_node(InstanceId,ClusterSpec,PodNode,NodesToConnect)||PodNode<-MissingConnectNodes],
    ok.


%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
missing_connect_nodes(InstanceId)->
    [Node||Node<-db_cluster_instance:nodes(connect,InstanceId), 
	   pang=:=net_adm:ping(Node)].
present_connect_nodes(InstanceId)->
    [Node||Node<-db_cluster_instance:nodes(connect,InstanceId), 
	   pong=:=net_adm:ping(Node)].
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
dbase_info(ClusterSpec,InstanceId)->
    {ok,ControllerHostSpecs}=db_cluster_spec:read(controller_host_specs,ClusterSpec),
    {ok,WorkerHostSpecs}=db_cluster_spec:read(worker_host_specs,ClusterSpec),
    ConnectHostSpecs=list_duplicates:remove(lists:append(ControllerHostSpecs,WorkerHostSpecs)),
    dbase_info(ConnectHostSpecs,InstanceId,ClusterSpec,[]).
    
dbase_info([],_InstanceId,_ClusterSpec,Acc)->
    ErrorList=[{error,[Reason]}||{error,[Reason]}<-Acc],
    case ErrorList of
	[]->
	    ok;
        Reason->
	    {error,Reason}
    end;
dbase_info([HostSpec|T],InstanceId,ClusterSpec,Acc)->
    {ok,ClusterDir}=db_cluster_spec:read(dir,ClusterSpec),
    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
    PodName=ClusterSpec++"_connect",
    PodNode=list_to_atom(PodName++"@"++HostName),
    PodDir=ClusterDir,
    Type=connect,
    Status=candidate,
    NewAcc=case db_cluster_instance:create(InstanceId,ClusterSpec,Type,PodName,PodNode,PodDir,HostSpec,Status) of
	       {atomic,ok}->
		   [ok|Acc];
	       Reason->
		   [{error,[Reason]}|Acc]
	   end,
    dbase_info(T,InstanceId,ClusterSpec,NewAcc).
     
%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
-define(TimeOut,10*1000).

connect_nodes(ClusterSpec,InstanceId)->
    NodesToConnect=db_cluster_instance:nodes(connect,InstanceId),
    MissingConnectNodes=[Node||Node<-NodesToConnect, 
			       pang=:=net_adm:ping(Node)],
    [create_connect_node(InstanceId,ClusterSpec,PodNode,NodesToConnect)||PodNode<-MissingConnectNodes].
    
create_connect_node(InstanceId,ClusterSpec,PodNode,NodesToConnect)->
    io:format("INFO: create new/restart connect_node  ~p~n",[{date(),time()}]), 
    io:format("INFO: Cluster and PodNode   ~p~n",[{ClusterSpec,PodNode}]),  
    
    {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
    {ok,HostSpec}=db_cluster_instance:read(host_spec,InstanceId,PodNode),
    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
    
    {ok,PodName}=db_cluster_instance:read(pod_name,InstanceId,PodNode),
    {ok,PodDir}=db_cluster_instance:read(pod_dir,InstanceId,PodNode),
    PaArgs=" -detached ",
    EnvArgs=" ",
    ops_vm:ssh_create(HostName,PodName,PodDir,Cookie,PaArgs,EnvArgs,NodesToConnect,?TimeOut).

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------


