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
 
-export([start/1]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(ClusterSpec,"prototype_c201").

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(ClusterSpec),
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

setup(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=application:set_env([{infra_service_app,[{cluster_spec,ClusterSpec}]}]),
    
    {ok,_}=db_etcd_server:start(),
    {ok,ClusterDir}=db_cluster_spec:read(dir,ClusterSpec),
    file:del_dir_r(ClusterDir),
    ok=file:make_dir(ClusterDir),

    db_etcd_server:stop(),
    
  
    ok=application:start(test_area),
      
    pong=db_etcd:ping(),
    pong=nodelog:ping(),
    pong=connect_server:ping(),
    pong=appl_server:ping(),
    pong=pod_server:ping(),
    pong=oam:ping(),
    pong=infra_service_server:ping(),

      
    ok.
