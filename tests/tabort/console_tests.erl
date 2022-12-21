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
-module(console_tests).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(ClusterSpec,"c200").

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(),
    ok=start_cluster_test(),
  %  ok=hw_conbee_app_test(),
    % ok=deploy_appls_test(),
    

  
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
hw_conbee_app_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ApplSpec="hw_conbee",
    HostSpec="c201",
    oam:new_appl(ApplSpec,HostSpec,60*1000),
    AllApps=oam:all_apps(),
    io:format("AllApps ~p~n",[{AllApps,?MODULE,?FUNCTION_NAME}]),
    
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
    
    R=console:new_cluster(?ClusterSpec),    
    io:format("R ~p~n",[{R,?MODULE,?FUNCTION_NAME}]),
  
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

setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    ok=application:set_env([{console,[{cluster_spec,?ClusterSpec}]}]),
    ok=application:set_env([{infra_service_app,[{cluster_spec,?ClusterSpec}]}]),
    
    
  %  {ok,_}=db_etcd_server:start(),
  %  db_etcd:install(),
  %  ok=db_appl_instance:create_table(),
  %  ok=db_cluster_instance:create_table(),
    
  %  {ok,ClusterDir}=db_cluster_spec:read(dir,?ClusterSpec),
  %  os:cmd("rm -rf "++ClusterDir),
  %  ok=file:make_dir(ClusterDir),
  %  {ok,_}=nodelog_server:start(),
  %  {ok,_}=resource_discovery_server:start(),
  %  {ok,_}=connect_server:start(),
  %  {ok,_}=appl_server:start(),
  %  {ok,_}=pod_server:start(),
  
    ok=application:start(console),
    ok.
