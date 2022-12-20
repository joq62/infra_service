%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(db_etcd_server).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([

	]).

-export([
	 start/0,
	 stop/0
	
	]).


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
	       }).

%% ====================================================================
%% External functions
%% ====================================================================

 
%% call
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).


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
    IntialNode=node(),
    lib_db_etcd:dynamic_install_start(IntialNode),
    
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  
    
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

handle_call({install},_From, State) ->

  %  io:format("DEBUG  ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=db_cluster_spec:create_table(),
    ClusterSpec=db_cluster_spec:git_clone_load(),
    Ok_ClusterSpec=[X||{ok,X}<-ClusterSpec],
    Err_ClusterSpec=[X||{error,X}<-ClusterSpec],

    ok=db_host_spec:create_table(),
    HostSpec=db_host_spec:git_clone_load(),
    Ok_HostSpec=[X||{ok,X}<-HostSpec],
    Err_HostSpec=[X||{error,X}<-HostSpec],

    ok=db_cluster_instance:create_table(),

    ok=db_appl_spec:create_table(),
    ApplSpec=db_appl_spec:git_clone_load(),
    Ok_ApplSpec=[X||{ok,X}<-ApplSpec],
    Err_ApplSpec=[X||{error,X}<-ApplSpec],

    ok=db_appl_deployment:create_table(),
    ApplDeployment=db_appl_deployment:git_clone_load(),
    Ok_ApplDeployment=[X||{ok,X}<-ApplDeployment],
    Err_ApplDeployment=[X||{error,X}<-ApplDeployment],

    ok=db_appl_instance:create_table(),
    

   

    Reply=[
	   {cluster_spec,Ok_ClusterSpec,Err_ClusterSpec},
	   {host_spec,Ok_HostSpec,Err_HostSpec},
	   {appl_spec,Ok_ApplSpec,Err_ApplSpec},
	   {appl_deployment,Ok_ApplDeployment,Err_ApplDeployment}],

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
handle_info(timeout, State) ->
    spawn(fun()->db_etcd:install() end),
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
