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
	 create_node/1,
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
create_node(PodNode)->
       gen_server:call(?MODULE, {create_node,PodNode},infinity).
create_pod(ParentNode,NodeName,PodDir,PaArgs,EnvArgs)->
    gen_server:call(?MODULE, {create_pod,ParentNode,NodeName,PodDir,PaArgs,EnvArgs},infinity).

desired_nodes()->
    gen_server:call(?MODULE,{desired_nodes},infinity).
active_nodes()->
    gen_server:call(?MODULE,{active_nodes},infinity).
stopped_nodes()->
    gen_server:call(?MODULE,{stopped_nodes},infinity).

%%----------------------------------------------------------------------

ping() ->
    gen_server:call(?MODULE, {ping}).
%% cast


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
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Servere started"]]), 
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
    sd:call(db_etcd,db_pod_desired_state,create_table,[],5000),
    Reply=case lib_pod:load_desired_state(ClusterSpec) of
	      ok->
		  NewState=State#state{cluster_spec=ClusterSpec},
		  ok;
	      {error,ErrorList}->
		  NewState=State,
		  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR:  when intite desired state : ",ErrorList]]),
		  {error,ErrorList}
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
		  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: Not initiated : ",undefined]]),
		  {error,["Not initiated : ",undefined]};
	      _ClusterSpec->
		  case lib_pod:desired_nodes() of
		      {error,Reason}->
			  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: desired nodes : ",Reason]]),
			  {error,Reason};
		      {ok,Nodes}->
			  {ok,Nodes}
		  end
	  end,
    {reply, Reply, State};

handle_call({active_nodes},_From, State) ->
    Reply=case State#state.cluster_spec of
	      undefined->
		  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: Not initiated : ",undefined]]),
		  {error,["Not initiated : ",undefined]};
	      _ClusterSpec->
		  case lib_pod:active_nodes() of
		      {error,Reason}->
			  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: active nodes : ",Reason]]),
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
		  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: Not initiated : ",undefined]]),
		  {error,["Not initiated : ",undefined]};
	      _ClusterSpec->
		  case lib_pod:stopped_nodes() of
		      {error,Reason}->
			  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: active nodes : ",Reason]]),
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
handle_call({create_node,PodNode},_From, State) ->
    Reply=lib_pod:create_node(PodNode),
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
    Reply=case sd:call(db_etcd,db_host_spec,read,[hostname,HostSpec],5000) of 
	      {error,Reason}->
		  {error,Reason};
	      {ok,HostName}->
		  case sd:call(db_etcd,db_appl_spec,read,[app,ApplSpec],5000) of
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
    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error Unmatched signal  : ",Request,?MODULE,?LINE]]),
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
    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error Unmatched signal  : ",Msg,?MODULE,?LINE]]),
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
    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error Unmatched signal  : ",Info,?MODULE,?LINE]]),
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


