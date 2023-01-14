%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(console_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(LocalTypes,[infra_service,oam,nodelog]).
-define(TargetTypes,[infra_service,oam,nodelog]).

%% --------------------------------------------------------------------

%% External exports
-export([
	 initiate/1,
	 load_desired_state/1,
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

initiate(ClusterSpec)->
    gen_server:call(?MODULE, {initiate,ClusterSpec},infinity).
load_desired_state(Type)->
    gen_server:call(?MODULE, {load_desired_state,Type},infinity).
    
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
    
    %-- Start needed servers
    {ok,_}=common_server:start(),
    pong=common_server:ping(),
    {ok,_}=resource_discovery_server:start(),
    pong=rd:ping(),
    ok=application:start(db_etcd),
    pong=db_etcd:ping(),

    %--
    ok=db_etcd:install(),
    %--
    {ok,_}=parent_server:start(),
    pong=parent_server:ping(),
    %--
    {ok,_}=pod_server:start(),
    pong=pod_server:ping(),
    %--
    {ok,_}=appl_server:start(),
    pong=appl_server:ping(),

  
    rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,"Servere started"]),

    {ok, #state{}}.   
 

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
handle_call({initiate,ClusterSpec},_From, State) ->
    Reply=case State#state.cluster_spec of
	      undefined->
		  {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
		  erlang:set_cookie(node(),list_to_atom(Cookie)),
		  NewState=State#state{cluster_spec=ClusterSpec},
		  ok;
	      CSpec->
		  NewState=State,
		  {errror,["Already intitaited : ",CSpec]}
	  end,	  
    {reply, Reply, NewState};

handle_call({load_desired_state,Type},_From, State) ->
    Reply=case State#state.cluster_spec of
	      undefined->
		  {error,["Not yet initiated with a cluster spec :"]};
	      ClusterSpec->
		  case Type of
		      parents->
			  parent_server:load_desired_state(ClusterSpec);
		      pods ->
			  pod_server:load_desired_state(ClusterSpec);
		      appls->
			  appl_server:load_desired_state(ClusterSpec);	
		      _ ->
			  {error,["Type not supported ",Type]}
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
