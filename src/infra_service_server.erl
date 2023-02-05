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
-define(LocalTypes,[infra_service]).
-define(TargetTypes,[nodelog]).

%% --------------------------------------------------------------------


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------
-record(state,{
	       cluster_spec	       
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

       

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
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Servere started",node()]]),
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
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DEBUG is_confi  : ",?MODULE_STRING,?LINE]]),
    Reply=case State#state.cluster_spec of 
	      undefined->
		  false;
	      _->
		  true
	  end,	  		      
    {reply, Reply, State};

handle_call({config,ClusterSpec},_From, State) ->
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DEBUG config,ClusterSpec  : ",config,ClusterSpec,?MODULE,?LINE]]),
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["DEBUG State#state.cluster_spec  : ",State#state.cluster_spec,?MODULE,?LINE]]),
    
    Reply=case State#state.cluster_spec of 
	      undefined->
		  case rpc:call(node(),lib_infra_service,init_servers,[ClusterSpec],2*60*1000) of
		      {badrpc,Reason}->
			  NewState=State,
			  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error when calling init_servers  : ",Reason,?MODULE,?LINE]]),
			  {error,["Error when calling init_servers :",Reason,?MODULE,?LINE]};
		      {error,Reason}->
			  NewState=State,
			  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error when calling init_servers  : ",Reason,?MODULE,?LINE]]),
			  {error,["Error when calling init_servers :",init_servers,?MODULE,?LINE]};
		      ok->
			  case rpc:cast(node(),lib_infra_service,start_orchistrate,[]) of
			      {badrpc,Reason}->
				  NewState=State,
				  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["Error when calling orchistrate  : ",Reason,?MODULE,?LINE]]),
				  {error,["Error when calling orchistrate :",Reason,?MODULE,?LINE]};
			      true->
				  NewState=State#state{cluster_spec=ClusterSpec},
				  ok
			  end
		  end;
	      AlreadyConfig->
		  NewState=State,
		  sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,[" Already configured  : ",AlreadyConfig,ClusterSpec,?MODULE,?LINE]]),
		  {error,["Error  Already configured:",ClusterSpec,?MODULE,?LINE]}
	  end,	  		      
    {reply, Reply, NewState};

handle_call({ping},_From, State) ->
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["ping",node()]]),
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


handle_cast({start_orchistrate}, State) ->
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["  : ",?MODULE,?LINE,?FUNCTION_NAME]]),
    rpc:cast(node(),lib_infra_service,orchistrate,[]),
    {noreply, State};


handle_cast({orchistrate_result,_ResultStartParentPods,
	     _ResultStartInfraAppls,_ResultStartUserAppls}, State) ->
    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["Start  : ",?MODULE,?LINE]]),

    
   % {ok,StoppedParents}=parent_server:stopped_nodes(),
  %  {ok,StoppedPods}=pod_server:stopped_nodes(),
  %  {ok,StoppedApplInfoLists}=appl_server:stopped_appls(),
   
    case parent_server:stopped_nodes() of
	{error,Reason2}->
	    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["StoppedParents  : ",Reason2,?MODULE,?LINE]]);
	{ok,[]}->
	    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["StoppedParents  : ",ok,?MODULE,?LINE]]);
	{ok,StoppedParents}->
	    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["StoppedParents  : ",StoppedParents,?MODULE,?LINE]])
    end,

    case pod_server:stopped_nodes() of
	{error,Reason1}->
	    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["StoppedPods  : ",Reason1,?MODULE,?LINE]]);
	{ok,[]}->
	    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["StoppedPods  : ",ok,?MODULE,?LINE]]);
	{ok,StoppedPods}->
	    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["StoppedPods  : ",StoppedPods,?MODULE,?LINE]])
    end,

  

    case appl_server:stopped_appls() of
	{error,Reason3}->
	    sd:cast(nodelog,nodelog,log,[warning,?MODULE_STRING,?LINE,["StoppedApplInfoLists  : ",Reason3,?MODULE,?LINE]]);
	{ok,[]}->
	    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["StoppedApplInfoLists  : ",ok,?MODULE,?LINE]]);
	{ok,StoppedApplInfoLists}->
	    sd:cast(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,["StoppedApplInfoLists  : ",StoppedApplInfoLists,?MODULE,?LINE]])
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
