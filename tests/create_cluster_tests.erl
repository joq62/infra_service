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
-module(create_cluster_tests).      
 
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
  
  
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

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
        
    AppEnv=[{ops_node,[{cluster_spec,?ClusterSpec}]}],
    Cookie=list_to_atom("cookie_"++?ClusterSpec),
    erlang:set_cookie(node(),Cookie),
    ConnectNodes=[list_to_atom(?ClusterSpec++"_connect@c200"),
		  list_to_atom(?ClusterSpec++"_connect@c201")],
    [rpc:call(ConnectNode,init,stop,[])||ConnectNode<-ConnectNodes],
    
    ok=application:set_env(AppEnv),
    ok=application:start(ops_node),
 %   pong=ops_node:ping(),
  %  pong=db_etcd:ping(),
%   io:format("setup  ~p~n",[{?MODULE,?LINE}]),
%    pong=ops_cluster_controller_server:ping(),
 %  kuk= io:format("setup  ~p~n",[{?MODULE,?LINE}]),
  %  pong=ops_application_controller_server:ping(),
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.
