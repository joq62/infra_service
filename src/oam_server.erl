%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(oam_server).
 
-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
-define(HeartbeatTime,20*1000).
-define(ApplTimeOut,2*5000).
-define(LocalTypes,[oam,nodelog]).
-define(TargetTypes,[nodelog]).


%% External exports


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
    
  
    {ok,ClusterSpec}=application:get_env(infra_service_app,cluster_spec),
    {ok,ClusterDir}=db_cluster_spec:read(dir,ClusterSpec),
    % Install nodelog
    LogDir="log_dir",
    LogFile="logs1.logs",
    LogDirPath=filename:join(ClusterDir,LogDir),

    case filelib:is_dir(LogDirPath) of
	true->
	    ok;
	false->
	    ok=file:make_dir(LogDirPath),
	    LogFilePath=filename:join(LogDirPath,LogFile),
	    ok=nodelog:create(LogFilePath)
    end,
       
    % set right cookie based on the ClusterSpec

    {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
    erlang:set_cookie(node(),list_to_atom(Cookie)),


    % Trade resources
    [rd:add_local_resource(Type,node())||Type<-?LocalTypes],
    [rd:add_target_resource_type(Type)||Type<-?TargetTypes],
    ok=rd:trade_resources(),
    timer:sleep(3000),
    io:format("Started Server ~p~n",[{?MODULE,?LINE}]), 
    rd:rpc_call(nodelog,nodelog,log,[notice,?MODULE_STRING,?LINE,"Servere started"]),
   
    {ok, #state{cluster_spec=ClusterSpec}}.   
 

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
%handle_call({start_nodelog,ClusterSpec,HostSpec,LogDir,LogFile},_From, State) ->
    
   % {ok,ClusterDir}=db_cluster_spec:read(dir,ClusterSpec),
   % HostSpec
   % PathLogDir=filename:join([ClusterDir,LogDir]),
   % os:cmd("rm -rf "++PathLogDir),
   % LogFilePath=filename:join([PathLogDir,LogFile]),
   % LogFile1=filename:join(["test_log_dir","logs","test1.logs"]),
   % ok=rpc:call(node(),nodelog,create,[LogFile1],5000),
 %   Reply=ok,
  %  {reply, Reply, State};

handle_call({is_cluster_deployed},_From, State) ->
    Reply=glurk_not_implmented,
    {reply, Reply, State};

handle_call({delete_cluster},_From, State) ->
    Reply=glurk_not_implemented,
    {reply, Reply, State};


handle_call({deploy_appls},_From, State) ->
    Reply=appl_server:deploy_appls(State#state.cluster_spec),
    {reply, Reply, State};


handle_call({new_appl,ApplSpec,HostSpec},_From, State) ->
    Reply= appl_server:new(ApplSpec,HostSpec,State#state.cluster_spec,?ApplTimeOut),
    {reply, Reply, State};


handle_call({new_appl,ApplSpec,HostSpec,TimeOut},_From, State) ->
    Reply= appl_server:new(ApplSpec,HostSpec,State#state.cluster_spec,TimeOut),
    {reply, Reply, State};

handle_call({delete_appl,AppSpec,PodNode},_From, State) ->
    Reply=appl_server:delete(AppSpec,PodNode),
  
    {reply, Reply, State};

handle_call({update_appl,AppSpec,PodNode,HostSpec},_From, State) ->
    Reply=case appl_server:delete(AppSpec,PodNode)of
	      {error,Reason}->
		  {error,Reason};
	      ok->
		  appl_server:new(AppSpec,HostSpec)
	  end,
		  
    {reply, Reply, State};

handle_call({new_db_info},_From, State) ->
    Reply=connect_server:create_dbase_info(State#state.cluster_spec),
    {reply, Reply, State};

handle_call({new_connect_nodes},_From, State) ->
    Reply=connect_server:create_connect_nodes(State#state.cluster_spec),
    {reply, Reply, State};

handle_call({new_controllers},_From, State) ->
    Reply=pod_server:create_controller_pods(State#state.cluster_spec),
    {reply, Reply, State};

handle_call({new_workers},_From, State) ->
    Reply=pod_server:create_worker_pods(State#state.cluster_spec),
    {reply, Reply, State};

handle_call({ping_connect_nodes},_From, State) ->
    ConnectNodes=db_cluster_instance:nodes(connect,State#state.cluster_spec),
    PingConnectNodes=[{net_adm:ping(Node),Node}||Node<-ConnectNodes],
    Reply={ok,PingConnectNodes},
    {reply, Reply, State};


handle_call({all_apps},_From, State) ->
    ControllerNodes=pod_server:present_controller_nodes(),
    WorkerNodes=pod_server:present_worker_nodes(),
    AllNodes=lists:append(ControllerNodes,WorkerNodes),
    Apps=[{Node,rpc:call(Node,net,gethostname,[],5*1000),rpc:call(Node,application,which_applications,[],5*1000)}||Node<-AllNodes],
    AllApps=[{Node,HostName,AppList}||{Node,{ok,HostName},AppList}<-Apps,
				      AppList/={badrpc,nodedown}],
    Reply={ok,AllApps},
    {reply, Reply, State};

handle_call({where_is_app,App},_From, State) ->
    ControllerNodes=pod_server:present_controller_nodes(),
    WorkerNodes=pod_server:present_worker_nodes(),
    AllNodes=lists:append(ControllerNodes,WorkerNodes),
    Apps=[{Node,rpc:call(Node,net,gethostname,[],5*1000),rpc:call(Node,application,which_applications,[],5*1000)}||Node<-AllNodes],
    AllApps=[{Node,HostName,AppList}||{Node,{ok,HostName},AppList}<-Apps,
				      AppList/={badrpc,nodedown}],
    HereTheyAre=[Node||{Node,_HostName,AppList}<-AllApps,
		       lists:keymember(App,1,AppList)],
    Reply={ok,HereTheyAre},
    {reply, Reply, State};

handle_call({present_apps},_From, State) ->
    PresentApps=appl_server:present_apps(State#state.cluster_spec),
    Reply={ok,PresentApps},						 
    {reply, Reply, State};

handle_call({missing_apps},_From, State) ->
    MissingApps=appl_server:missing_apps(State#state.cluster_spec),
    Reply={ok,MissingApps},
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
