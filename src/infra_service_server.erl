%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(infra_service_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(LocalTypes,[infra_service,oam,nodelog]).
-define(TargetTypes,[infra_service,oam,nodelog]).

%% --------------------------------------------------------------------

%% External exports
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

ping() ->
    gen_server:call(?MODULE, {ping}).

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
     rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,"Servere started"]),
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
handle_call({is_config},_From, State) ->
    Reply=case State#state.cluster_spec of 
	      undefined->
		  false;
	      _->
		  true
	  end,	  		      
    {reply, Reply, State};

handle_call({config,ClusterSpec},_From, State) ->
    Reply=case State#state.cluster_spec of 
	      undefined->
		  case rpc:cast(node(),lib_infra_service,orchistrate,[]) of
		      {badrpc,Reason}->
			  NewState=State,
			  {error,["Error when calling orchistrate :",Reason,?MODULE,?LINE]};
		      ok->
			  NewState=State#state{cluster_spec=ClusterSpec},
			  ok
		  end;
	      _->
		  NewState=State,
		  {error,["Error  Already configured:",ClusterSpec,?MODULE,?LINE]}
	  end,	  		      
    {reply, Reply, NewState};

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

handle_cast({orchistrate_result,_ResultStartParentPods,
	     _ResultStartInfraAppls,_ResultStartUserAppls}, State) ->
    {ok,StoppedParents}=parent_server:stopped_nodes(),
    {ok,StoppedPods}=pod_server:stopped_nodes(),
    {ok,StoppedApplInfoLists}=appl_server:stopped_appls(),

    case {StoppedParents,StoppedPods,StoppedApplInfoLists} of
	{[],[],[]}->
	    ok;
	_->
	    io:format("StoppedParents ~p~n",[{StoppedParents,?MODULE,?LINE}]),
	    io:format("StoppedPods ~p~n",[{StoppedPods,?MODULE,?LINE}]),
	    io:format("StoppedApplInfoLists ~p~n",[{StoppedApplInfoLists,?MODULE,?LINE}])
    end,
    rpc:cast(node(),lib_infra_service,orchistrate,[]),
    {noreply, State};

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
