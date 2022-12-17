%%% -------------------------------------------------------------------
%%% @author  : Joq Erlang
%%% @doc: : 
%%% Created :
%%% Node end point  
%%% Creates and deletes Pods
%%% 
%%% API-kube: Interface 
%%% Pod consits beams from all services, app and app and sup erl.
%%% The setup of envs is
%%% -------------------------------------------------------------------
-module(oam_ext_test).       
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(),
    ok=start_cluster_test(),
    ok=deploy_appls_test(),
    

  
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
deploy_appls_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    ok=oam:deploy_appls(),
    _AllApps=oam:all_apps(),
  %  io:format("AllApps ~p~n",[{AllApps,?MODULE,?FUNCTION_NAME}]),
    {ok,HereIsIt}=oam:where_is_app(math),
  %  io:format("HereIsIt ~p~n",[{HereIsIt,?MODULE,?FUNCTION_NAME}]),
    [PodNode|_]=HereIsIt,
    42=rpc:call(PodNode,test_add,add,[20,22],2000),

    {ok,PresentApps}=oam:present_apps(),
    io:format("PresentApps ~p~n",[{PresentApps,?MODULE,?FUNCTION_NAME}]),
    {ok,MissingApps}=oam:missing_apps(),
    io:format("MissingApps ~p~n",[{MissingApps,?MODULE,?FUNCTION_NAME}]),
    
    
    

    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
%%-----------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start_cluster_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    
    glurk_not_implmented=oam:is_cluster_deployed(),
    []=oam:new_connect_nodes(),

    ok=oam:new_db_info(),
    []=oam:new_connect_nodes(),
    {ok,PingNodes}=oam:ping_connect_nodes(),
    [{pong,'c200_c201_connect@c200'},
     {pong,'c200_c201_connect@c201'}
    ]=lists:sort(PingNodes),
    ok=oam:new_db_info(),
    
    {PresentControllers,MissingControllers}=oam:new_controllers(),
    [
     '1_c200_c201_controller@c201',
     '2_c200_c201_controller@c200'
    ]= lists:sort(PresentControllers),
    []=MissingControllers,
   
    {PresentWorkers,MissingWorkers}=oam:new_workers(),
    [
     '1_c200_c201_worker@c201','2_c200_c201_worker@c200',
     '3_c200_c201_worker@c201','4_c200_c201_worker@c200',
     '5_c200_c201_worker@c201','6_c200_c201_worker@c200'
    ]=lists:sort(PresentWorkers),
    []=MissingWorkers,
    
  
    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
-define(ClusterSpec,"c200_c201").
setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    ok=application:set_env([{oam,[{cluster_spec,?ClusterSpec}]}]),
    
    
    {ok,_}=db_etcd_server:start(),
    db_etcd:install(),
    ok=db_appl_instance:create_table(),
    ok=db_cluster_instance:create_table(),
    
    {ok,ClusterDir}=db_cluster_spec:read(dir,?ClusterSpec),
    os:cmd("rm -rf "++ClusterDir),
    ok=file:make_dir(ClusterDir),
    {ok,_}=nodelog_server:start(),
    {ok,_}=resource_discovery_server:start(),
    {ok,_}=connect_server:start(),
    {ok,_}=appl_server:start(),
    {ok,_}=pod_server:start(),
  
    ok=application:start(oam),
    ok.
