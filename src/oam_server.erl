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

%% External exports
-export([
	 new_appl/3,
	 update_appl/4,
	 delete_appl/3,
	 deploy_appls/1,
	 
	 
	 new_cluster/1,
	 delete_cluster/1,
	 is_cluster_deployed/1,
	 ping_connect_nodes/1,

	 new_db_info/1,
	 new_connect_nodes/1,
	 new_controllers/1,
	 new_workers/1,
	 

	 ping/0
	]).

%debug
-export([
	 all_apps/1,
	 present_apps/1,
	 missing_apps/1,
	 call/6,
	 where_is_app/2
	]).

-export([
	 start/0,
	 stop/0
	]).


%% gen_server callbacks



-export([init/1, handle_call/3,handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%-------------------------------------------------------------------
-record(state,{
	       cluster_specs
	      }).


%% ====================================================================
%% External functions
%% ====================================================================

	    
%% call
start()-> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop()-> gen_server:call(?MODULE, {stop},infinity).

new_appl(ApplSpec,HostSpec,ClusterSpec)->
    gen_server:call(?MODULE, {new_appl,ApplSpec,HostSpec,ClusterSpec},infinity).
update_appl(ApplSpec,PodNode,HostSpec,ClusterSpec)->
    gen_server:call(?MODULE, {update_appl,ApplSpec,PodNode,HostSpec,ClusterSpec},infinity).
delete_appl(ApplSpec,PodNode,ClusterSpec)->
    gen_server:call(?MODULE, {delete_appl,ApplSpec,PodNode,ClusterSpec},infinity).
deploy_appls(ClusterSpec)->
    gen_server:call(?MODULE, {deploy_appls,ClusterSpec},infinity).


new_db_info(ClusterSpec)->
    gen_server:call(?MODULE, {new_db_info,ClusterSpec},infinity).
new_connect_nodes(ClusterSpec)->
    gen_server:call(?MODULE, {new_connect_nodes,ClusterSpec},infinity).
new_controllers(ClusterSpec)->
    gen_server:call(?MODULE, {new_controllers,ClusterSpec},infinity).
new_workers(ClusterSpec)->
    gen_server:call(?MODULE, {new_workers,ClusterSpec},infinity).
	 
new_cluster(ClusterSpec)->
    gen_server:call(?MODULE, {new_cluster,ClusterSpec},infinity).
delete_cluster(ClusterInstance)->
    gen_server:call(?MODULE, {delete_cluster,ClusterInstance},infinity).

is_cluster_deployed(ClusterSpec)->
    gen_server:call(?MODULE, {is_cluster_deployed,ClusterSpec},infinity).

%% debug

all_apps(ClusterSpec)->
    gen_server:call(?MODULE, {all_apps,ClusterSpec},infinity).

present_apps(ClusterSpec) ->
    gen_server:call(?MODULE, {present_apps,ClusterSpec}).

missing_apps(ClusterSpec) ->
    gen_server:call(?MODULE, {missing_apps,ClusterSpec}).

where_is_app(ClusterSpec,App)->
    gen_server:call(?MODULE, {where_is_app,ClusterSpec,App},infinity).
call(ClusterSpec,PodNode,M,F,A,T)->
    gen_server:call(?MODULE, {call,ClusterSpec,PodNode,M,F,A,T},infinity).

ping_connect_nodes(ClusterSpec)->
    gen_server:call(?MODULE, {ping_connect_nodes,ClusterSpec},infinity).

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
   
    db_etcd:install(),
    ok=db_appl_instance:create_table(),
    ok=db_cluster_instance:create_table(),
    
   
    {ok, #state{ cluster_specs=[]}}.   
 

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
handle_call({deploy_appls,ClusterSpec},_From, State) ->
    Reply= case lists:keyfind(ClusterSpec,1,State#state.cluster_specs) of
	       false->
		   {error,[eexists,ClusterSpec,?MODULE,?LINE]};
	       {ClusterSpec,InstanceId}->
		   CurrentCookie=erlang:get_cookie(),
		   {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
		   erlang:set_cookie(node(),list_to_atom(Cookie)),
		   R=appl_server:deploy_appls(ClusterSpec,InstanceId),
		   erlang:set_cookie(node(),CurrentCookie),
		   R
	end,
    {reply, Reply, State};


handle_call({new_appl,ApplSpec,HostSpec,ClusterSpec},_From, State) ->
    Reply= case lists:keyfind(ClusterSpec,1,State#state.cluster_specs) of
	       false->
		   {error,[eexists,ClusterSpec,?MODULE,?LINE]};
	       {ClusterSpec,InstanceId}->
		   CurrentCookie=erlang:get_cookie(),
		   {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
		   erlang:set_cookie(node(),list_to_atom(Cookie)),
		   R=appl_server:new(ApplSpec,HostSpec,ClusterSpec,InstanceId),
		   erlang:set_cookie(node(),CurrentCookie),
		   R
	end,
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

handle_call({new_db_info,ClusterSpec},_From, State) ->
    Reply=case lists:keymember(ClusterSpec,1,State#state.cluster_specs) of
	      true->
		  NewState=State,
		  {error,[already_created,ClusterSpec]};
	      false->
		  InstanceId=erlang:integer_to_list(os:system_time(microsecond),36)++"_id",
		  case connect_server:create_dbase_info(ClusterSpec,InstanceId) of
		      ok->
			  NewState=State#state{cluster_specs=[{ClusterSpec,InstanceId}|State#state.cluster_specs]},
			  ok;
		      {error,Reason} ->
			  NewState=State,
			  {error,Reason} 
		  end
	  end,
 {reply, Reply, NewState};

handle_call({new_connect_nodes,ClusterSpec},_From, State) ->
    Reply= case lists:keyfind(ClusterSpec,1,State#state.cluster_specs) of
	       false->
		   NewState=State,
		   {error,[eexists,ClusterSpec,?MODULE,?LINE]};
	       {ClusterSpec,InstanceId}->
		   CurrentCookie=erlang:get_cookie(),
		   {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
		   erlang:set_cookie(node(),list_to_atom(Cookie)),
		   connect_server:create_connect_nodes(ClusterSpec,InstanceId),
		   erlang:set_cookie(node(),CurrentCookie),
		   NewState=State#state{cluster_specs=[{ClusterSpec,InstanceId}|State#state.cluster_specs]},
		   ok
	  end,
 {reply, Reply, NewState};

handle_call({new_controllers,ClusterSpec},_From, State) ->
    Reply= case lists:keyfind(ClusterSpec,1,State#state.cluster_specs) of
	       false->
		   {error,[eexists,ClusterSpec,?MODULE,?LINE]};
	       {ClusterSpec,InstanceId}->
		   CurrentCookie=erlang:get_cookie(),
		   {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
		   erlang:set_cookie(node(),list_to_atom(Cookie)),
		   pod_server:create_controller_pods(ClusterSpec,InstanceId),
		   erlang:set_cookie(node(),CurrentCookie),
		   ok
	   end,
    {reply, Reply, State};

handle_call({new_workers,ClusterSpec},_From, State) ->
    Reply= case lists:keyfind(ClusterSpec,1,State#state.cluster_specs) of
	       false->
		   {error,[eexists,ClusterSpec,?MODULE,?LINE]};
	       {ClusterSpec,InstanceId}->
		   CurrentCookie=erlang:get_cookie(),
		   {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
		   erlang:set_cookie(node(),list_to_atom(Cookie)),
		   pod_server:create_worker_pods(ClusterSpec,InstanceId),
		   erlang:set_cookie(node(),CurrentCookie),
		   ok
	   end,
    {reply, Reply, State};

handle_call({ping_connect_nodes,ClusterSpec},_From, State) ->
    Reply= case lists:keyfind(ClusterSpec,1,State#state.cluster_specs) of
	       false->
		   {error,[eexists,ClusterSpec,?MODULE,?LINE]};
	       {ClusterSpec,InstanceId}->
		   CurrentCookie=erlang:get_cookie(),
		   {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
		   erlang:set_cookie(node(),list_to_atom(Cookie)),
		   ConnectNodes=db_cluster_instance:nodes(connect,InstanceId),
		   PingConnectNodes=[{net_adm:ping(Node),Node}||Node<-ConnectNodes],
		   erlang:set_cookie(node(),CurrentCookie),
		  
		   {ok,PingConnectNodes}
	   end,
    {reply, Reply, State};

handle_call({is_cluster_deployed,ClusterSpec},_From, State) ->
    Reply=lists:keymember(ClusterSpec,1,State#state.cluster_specs),
    {reply, Reply, State};

handle_call({delete_cluster,_ClusterInstance},_From, State) ->
    Reply=pong,
    {reply, Reply, State};


handle_call({all_apps,ClusterSpec},_From, State) ->
    Reply= case lists:keyfind(ClusterSpec,1,State#state.cluster_specs) of
	       false->
		   {error,[eexists,ClusterSpec,?MODULE,?LINE]};
	       {ClusterSpec,_InstanceId}->
		   CurrentCookie=erlang:get_cookie(),
		   {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
		   erlang:set_cookie(node(),list_to_atom(Cookie)),
		   ControllerNodes=pod_server:present_controller_nodes(),
		   WorkerNodes=pod_server:present_worker_nodes(),
		   AllNodes=lists:append(ControllerNodes,WorkerNodes),
		   Apps=[{Node,rpc:call(Node,net,gethostname,[],5*1000),rpc:call(Node,application,which_applications,[],5*1000)}||Node<-AllNodes],
		   AllApps=[{Node,HostName,AppList}||{Node,{ok,HostName},AppList}<-Apps,
							    AppList/={badrpc,nodedown}],
		  
		   erlang:set_cookie(node(),CurrentCookie),
		   {ok,AllApps}
	    end,
    {reply, Reply, State};

handle_call({where_is_app,ClusterSpec,App},_From, State) ->
    Reply= case lists:keyfind(ClusterSpec,1,State#state.cluster_specs) of
	       false->
		   {error,[eexists,ClusterSpec,?MODULE,?LINE]};
	       {ClusterSpec,_InstanceId}->
		   CurrentCookie=erlang:get_cookie(),
		   {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
		   erlang:set_cookie(node(),list_to_atom(Cookie)),
		   ControllerNodes=pod_server:present_controller_nodes(),
		   WorkerNodes=pod_server:present_worker_nodes(),
		   AllNodes=lists:append(ControllerNodes,WorkerNodes),
		   Apps=[{Node,rpc:call(Node,net,gethostname,[],5*1000),rpc:call(Node,application,which_applications,[],5*1000)}||Node<-AllNodes],
		   AllApps=[{Node,HostName,AppList}||{Node,{ok,HostName},AppList}<-Apps,
						     AppList/={badrpc,nodedown}],
		   HereTheyAre=[Node||{Node,_HostName,AppList}<-AllApps,
				      lists:keymember(App,1,AppList)],
		   erlang:set_cookie(node(),CurrentCookie),
		   {ok,HereTheyAre}
	    end,
    {reply, Reply, State};

handle_call({present_apps,ClusterSpec},_From, State) ->
    Reply= case lists:keyfind(ClusterSpec,1,State#state.cluster_specs) of
	       false->
		   {error,[eexists,ClusterSpec,?MODULE,?LINE]};
	       {ClusterSpec,InstanceId}->
		   PresentApps=appl_server:present_apps(InstanceId),
		   {ok,PresentApps}
	    end,
    {reply, Reply, State};

handle_call({missing_apps,ClusterSpec},_From, State) ->
    Reply= case lists:keyfind(ClusterSpec,1,State#state.cluster_specs) of
	       false->
		   {error,[eexists,ClusterSpec,?MODULE,?LINE]};
	       {ClusterSpec,InstanceId}->
		   PresentApps=appl_server:missing_apps(InstanceId),
		   {ok,PresentApps}
	    end,
    {reply, Reply, State};

handle_call({call,ClusterSpec,PodNode,M,F,A,T},_From, State) ->
    Reply= case lists:keyfind(ClusterSpec,1,State#state.cluster_specs) of
	       false->
		   {error,[eexists,ClusterSpec,?MODULE,?LINE]};
	       {ClusterSpec,_InstanceId}->
		   CurrentCookie=erlang:get_cookie(),
		   {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
		   erlang:set_cookie(node(),list_to_atom(Cookie)),
		   Result=rpc:call(PodNode,M,F,A,T),
		   erlang:set_cookie(node(),CurrentCookie),
		   {ok,Result}
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
