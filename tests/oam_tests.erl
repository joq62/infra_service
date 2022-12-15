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
-module(oam_tests).      
 
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
  %  ok=test_3(),

  
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
deploy_appls_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
%     timer:sleep(60*1000),
    ClusterSpec="c200_c201",
   
    ok=oam_server:deploy_appls(ClusterSpec),
    AllApps=oam_server:all_apps(ClusterSpec),
  %  io:format("AllApps ~p~n",[{AllApps,?MODULE,?FUNCTION_NAME}]),
    {ok,HereIsIt}=oam_server:where_is_app(ClusterSpec,math),
  %  io:format("HereIsIt ~p~n",[{HereIsIt,?MODULE,?FUNCTION_NAME}]),
    [PodNode|_]=HereIsIt,
    {ok,42}=oam_server:call(ClusterSpec,PodNode,test_add,add,[20,22],2000),

    {ok,PresentApps}=oam_server:present_apps(ClusterSpec),
    io:format("PresentApps ~p~n",[{PresentApps,?MODULE,?FUNCTION_NAME}]),
    {ok,MissingApps}=oam_server:missing_apps(ClusterSpec),
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

    ClusterSpec1="c200_c201",
    false=oam_server:is_cluster_deployed(ClusterSpec1),
    {error,[eexists,"c200_c201",oam_server,_]}=oam_server:new_connect_nodes(ClusterSpec1),

    ok=oam_server:new_db_info(ClusterSpec1),
    ok=oam_server:new_connect_nodes(ClusterSpec1),
    {ok,PingNodes}=oam_server:ping_connect_nodes(ClusterSpec1),
    [{pong,'c200_c201_connect@c200'},
     {pong,'c200_c201_connect@c201'}
    ]=lists:sort(PingNodes),
    {error,[already_created,"c200_c201"]}=oam_server:new_db_info(ClusterSpec1),
    ok=oam_server:new_controllers(ClusterSpec1),
    ok=oam_server:new_workers(ClusterSpec1),
    
    true=oam_server:is_cluster_deployed(ClusterSpec1),
 %   ClusterSpec2="single_c200",
 %   ok=oam_server:new_db_info(ClusterSpec2),
 %   ok=oam_server:new_connect_nodes(ClusterSpec2),
 %   {ok,PingNodes2}=oam_server:ping_connect_nodes(ClusterSpec2),
 %   [{pong,'single_c200_connect@c200'}]=lists:sort(PingNodes2),
  
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
    {ok,_}=db_etcd_server:start(),
    {ok,_}=resource_discovery_server:start(),
    {ok,_}=connect_server:start(),
    {ok,_}=appl_server:start(),
    {ok,_}=pod_server:start(),
 %   
    {ok,_}=oam_server:start(),

    ok.
