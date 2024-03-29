%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(parent_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(HeartbeatTime,20*1000).

 

%% External exports





-export([
	 load_desired_state/1,
	 create_node/1,
	 desired_nodes/0,
	 active_nodes/0,
	 stopped_nodes/0

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
	       cluster_spec
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

	    
%% call
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).
%%---------------------------------------------------------------------
load_desired_state(ClusterSpec)->
    gen_server:call(?MODULE,{load_desired_state,ClusterSpec},infinity).

create_node(ParentNode)->
    gen_server:call(?MODULE,{create_node,ParentNode},infinity).

desired_nodes()->
    gen_server:call(?MODULE,{desired_nodes},infinity).
active_nodes()->
    gen_server:call(?MODULE,{active_nodes},infinity).
stopped_nodes()->
    gen_server:call(?MODULE,{stopped_nodes},infinity).


%%----------------------------------------------------------------------------

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
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Servere started"]]),
    {ok, #state{cluster_spec=undefined}}.   
 

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
handle_call({load_desired_state,ClusterSpec},_From, State) ->
    Reply=case lib_parent:load_desired_state(ClusterSpec) of
	      ok->
		  NewState=State#state{cluster_spec=ClusterSpec},
		  ok;
	      {error,ErrorList}->
		  NewState=State,
		  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR:  when intite desired state : ",ErrorList]]),
		  {error,ErrorList}
	  end,
    {reply, Reply, NewState};

handle_call({create_node,ParentNode},_From, State) ->
    Reply=case State#state.cluster_spec of
	      undefined->
		  {error,["Not initiated : ",undefined]};
	      _ClusterSpec->
		  case lib_parent:create_node(ParentNode) of
		      ok->
			  ok;
		      {error,Reason}->
			  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: creating node : ",ParentNode,Reason]]),
			  {error,Reason}
		  end
	  end,
    {reply, Reply, State};

handle_call({desired_nodes},_From, State) ->
    Reply=case State#state.cluster_spec of
	      undefined->
		  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: Not initiated : ",undefined]]),
		  {error,["Not initiated : ",undefined]};
	      _ClusterSpec->
		  case lib_parent:desired_nodes() of
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
		  case lib_parent:active_nodes() of
		      {error,Reason}->
			  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: active nodes : ",Reason]]),
			  {error,Reason};
		      {ok,Nodes}->
			  {ok,Nodes}
		  end
	  end,
    {reply, Reply, State};

handle_call({stopped_nodes},_From, State) ->
    Reply=case State#state.cluster_spec of
	      undefined->
		  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: Not initiated : ",undefined]]),
		  {error,["Not initiated : ",undefined]};
	      _ClusterSpec->
		  case lib_parent:stopped_nodes() of
		      {error,Reason}->
			  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["ERROR: active nodes : ",Reason]]),
			  {error,Reason};
		      {ok,Nodes}->
			  {ok,Nodes}
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
