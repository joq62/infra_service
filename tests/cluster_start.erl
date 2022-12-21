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
-module(cluster_start).      
 
-export([start/0]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(ClusterSpec,"prototype_c201").

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(),
    ok=start_cluster_test(),
        
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  %  init:stop(),
  %  timer:sleep(2000),
    ok.

%%-----------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start_cluster_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
       
    
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
    
    ok=application:set_env([{infra_service_app,[{cluster_spec,?ClusterSpec}]}]),
     
    {ok,_}=db_etcd_server:start(),
 %   db_etcd:install(),
 %   ok=db_appl_instance:create_table(),
  %  ok=db_cluster_instance:create_table(),
    
    {ok,ClusterDir}=db_cluster_spec:read(dir,?ClusterSpec),
    os:cmd("rm -rf "++ClusterDir),
    ok=file:make_dir(ClusterDir),
    {ok,_}=nodelog_server:start(),
    {ok,_}=resource_discovery_server:start(),
    {ok,_}=connect_server:start(),
    {ok,_}=appl_server:start(),
    {ok,_}=pod_server:start(),
    {ok,_}=oam_server:start(),
    {ok,_}=infra_service_server:start(),
 %   ok=application:start(_app),
    timer:sleep(3000),

    %Start cluster 
  %  io:format("Starts cluster spec  ~p~n",[{?ClusterSpec,?MODULE,?FUNCTION_NAME}]),
    
  %  {ok,_}=oam:new_db_info(),    
  %  ok=oam:new_connect_nodes(),
    
 %   {PresentControllers,MissingControllers}=oam:new_controllers(),
  %  {PresentWorkers,MissingWorkers}=oam:new_workers(),
    
  %  io:format("PresentControllers,MissingControllers ~p~n",[{PresentControllers,MissingControllers,?MODULE,?FUNCTION_NAME}]),
   % io:format("PresentWorkers,MissingWorkers ~p~n",[{PresentWorkers,MissingWorkers,?MODULE,?FUNCTION_NAME}]),

    
    
    ok.
