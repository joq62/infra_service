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
-module(st_cl).      
 
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
       
    {ok,_}=oam:new_db_info(),
    oam:new_connect_nodes(),
    {ok,[{pong,'prototype_c201_connect@c201'}]}=oam:ping_connect_nodes(),
    
    %[
    % {pong,'"prototype_c201_connect@c201'}
    %]=lists:sort(PingNodes),
    
    {PresentControllers,MissingControllers}=oam:new_controllers(),
 %   io:format("PresentControllers,MissingControllers ~p~n",[{PresentControllers,MissingControllers,?MODULE,?FUNCTION_NAME}]),

    ['1_prototype_c201_controller@c201']= lists:sort(PresentControllers),
    []=MissingControllers,
   
    {PresentWorkers,MissingWorkers}=oam:new_workers(),
  %  io:format("PresentWorkers,MissingWorkers ~p~n",[{PresentWorkers,MissingWorkers,?MODULE,?FUNCTION_NAME}]),
    
    [
     '1_prototype_c201_worker@c201','2_prototype_c201_worker@c201'
    
    ]=lists:sort(PresentWorkers),
    []=MissingWorkers,

  %  Nodes=[{Node,rpc:call(Node,erlang,nodes,[],2000)}||Node<-nodes()],
   % io:format(" Nodes ~p~n",[{Nodes,?MODULE,?FUNCTION_NAME}]),
    
  
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
-define(ClusterSpec,"prototype_c201").
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
    timer:sleep(3000),

    ok.
