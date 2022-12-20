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
-module(appl_tests).      
 
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
    ok=load_start_math(),
    ok=check_present_missing(),
  				
     
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
load_start_math()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ApplSpec="math",
    HostSpec="c201",

    TimeOut=10*1000,

    {ok,PodNode}=appl_server:new(ApplSpec,HostSpec,?ClusterSpec,TimeOut),
    42=rpc:call(PodNode,test_add,add,[20,22],2000),

    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
check_present_missing()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
  %  io:format("Apps ~p~n",[[rpc:call(Node,application,which_applications,[],2000)||Node<-nodes()]]),
    [{"math",PodNode,math}]=appl_server:present_apps(?ClusterSpec),
 %   io:format("Presents ~p~n",[{appl_server:present_apps(?ClusterSpec),?FUNCTION_NAME,?LINE}]),
  %  io:format("Missing ~p~n",[{appl_server:missing_apps(?ClusterSpec),?FUNCTION_NAME,?LINE}]),

    rpc:call(PodNode,application,stop,[math],5000),
    rpc:call(PodNode,application,unload,[math],5000),

  %  io:format("Presents ~p~n",[{appl_server:present_apps(?ClusterSpec),?FUNCTION_NAME,?LINE}]),
  %  io:format("Missing ~p~n",[{appl_server:missing_apps(?ClusterSpec),?FUNCTION_NAME,?LINE}]),
    
    
    



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

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    pod_server:start_monitoring(?ClusterSpec),
    appl_server:start_monitoring(?ClusterSpec),

    ok.
