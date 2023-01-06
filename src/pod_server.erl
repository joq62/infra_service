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
	 create_pod/5,
	 load_desired_state/1,
	 desired_nodes/0,
	 active_nodes/0,
	 stopped_nodes/0,
	 
	 ping/0
	]).


-export([

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


%create_pod(ClusterSpec,HostSpec,Type)->
 %   gen_server:call(?MODULE, {create_pod,ClusterSpec,HostSpec,Type},infinity).

%get_pod(HostSpec,Type)->
 %     gen_server:call(?MODULE, {get_pod,HostSpec,Type},infinity).
%-----------------------------------------------------------------------
load_desired_state(ClusterSpec)->
    gen_server:call(?MODULE,{load_desired_state,ClusterSpec},infinity).

create_pod(ParentNode,NodeName,PodDir,PaArgs,EnvArgs)->
    gen_server:call(?MODULE, {create_pod,ParentNode,NodeName,PodDir,PaArgs,EnvArgs},infinity).

desired_nodes()->
    gen_server:call(?MODULE,{desired_nodes},infinity).
active_nodes()->
    gen_server:call(?MODULE,{active_nodes},infinity).
stopped_nodes()->
    gen_server:call(?MODULE,{stopped_nodes},infinity).

%%----------------------------------------------------------------------


get_pod(ApplSpec,HostSpec)->
      gen_server:call(?MODULE, {get_pod,ApplSpec,HostSpec},infinity).
get_pod(ApplSpec,HostSpec,Type)->
      gen_server:call(?MODULE, {get_pod,ApplSpec,HostSpec,Type},infinity).

create_controller_pods(ClusterSpec)->
    gen_server:call(?MODULE, {create_controller_pods,ClusterSpec},infinity).

create_worker_pods(ClusterSpec)->
    gen_server:call(?MODULE, {create_worker_pods,ClusterSpec},infinity).

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
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

handle_call({load_desired_state,ClusterSpec},_From, State) ->
    Reply=case State#state.cluster_spec of
	      undefined->
		  ok=db_pod_desired_state:create_table(),
		  case lib_pod:load_desired_state(ClusterSpec) of
		      ok->
			  rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["OK: initiation of desired state : ",ClusterSpec]]),
			  NewState=State#state{cluster_spec=ClusterSpec},
			  ok;
		      {error,ErrorList}->
			  NewState=State,
			  rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR:  when intite desired state : ",ErrorList]]),
			  {error,ErrorList}
		  end;
	      ClusterSpec->
		  NewState=State,
		  rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: Already initiated : ",ClusterSpec]]),
		  {error,["Already initiated : ",ClusterSpec]};
	      AnotherCluster->
		  NewState=State,
		  rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: Already initiated : ",AnotherCluster]]),
		  {error,["Already initiated : ",AnotherCluster]}
	  end,
    {reply, Reply, NewState};

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

handle_call({desired_nodes},_From, State) ->
    Reply=case State#state.cluster_spec of
	      undefined->
		  rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: Not initiated : ",undefined]]),
		  {error,["Not initiated : ",undefined]};
	      _ClusterSpec->
		  case lib_pod:desired_nodes() of
		      {error,Reason}->
			  rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: desired nodes : ",Reason]]),
			  {error,Reason};
		      {ok,Nodes}->
			  {ok,Nodes}
		  end
	  end,
    {reply, Reply, State};

handle_call({active_nodes},_From, State) ->
    Reply=case State#state.cluster_spec of
	      undefined->
		  rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: Not initiated : ",undefined]]),
		  {error,["Not initiated : ",undefined]};
	      _ClusterSpec->
		  case lib_pod:active_nodes() of
		      {error,Reason}->
			  rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: active nodes : ",Reason]]),
			  {error,Reason};
		      {ok,Nodes}->
			  {ok,Nodes}
		  end
	  end,
    {reply, Reply, State};
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

handle_call({stopped_nodes},_From, State) ->
    Reply=case State#state.cluster_spec of
	      undefined->
		  rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: Not initiated : ",undefined]]),
		  {error,["Not initiated : ",undefined]};
	      _ClusterSpec->
		  case lib_pod:stopped_nodes() of
		      {error,Reason}->
			  rd:rpc_call(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: active nodes : ",Reason]]),
			  {error,Reason};
		      {ok,Nodes}->
			  {ok,Nodes}
		  end
	  end,
    {reply, Reply, State};
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

handle_call({create_pod,ParentNode,NodeName,Dir,PaArgs,EnvArgs},_From, State) ->
    Reply=lib_pod:create_node(ParentNode,NodeName,Dir,PaArgs,EnvArgs),
    {reply, Reply, State};
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

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


