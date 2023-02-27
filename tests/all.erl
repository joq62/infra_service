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
-module(all).      
    
 
-export([start/1,
	 notice/0,warning/0,alert/0,
	 stop/0,stop/1,
	 all_apps/0,
	 
	 print/2]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
stop()->
    stop(c200_c201_parent@c201).

stop(N)->
    rpc:call(N,init,stop,[],5000).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


all_apps()->
    [InfraPod|_]=sd:get_node(infra_service),
    running_apps(InfraPod).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
notice()->
    sd:call(nodelog,nodelog,read,[notice],2000).
warning()->
    sd:call(nodelog,nodelog,read,[warning],2000).
alert()->
    sd:call(nodelog,nodelog,read,[alert],2000).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
print(Arg1,Arg2)->
    io:format(Arg1,Arg2).
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start([ClusterSpec,HostSpec])->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    ok=setup(ClusterSpec),
    ok=infra_service_test:setup(),
    ok=local:start_local(ClusterSpec),
    io:format("cookie ~p~n",[{erlang:get_cookie(),?MODULE,?LINE}]),
 % just for testing 
    [rpc:call(Pod,init,stop,[],5000)||Pod<-db_parent_desired_state:get_all_id()],
    timer:sleep(2000),
    [rpc:call(Pod,init,stop,[],5000)||Pod<-db_pod_desired_state:get_all_id()],
    timer:sleep(1000),
    io:format("OK: local:start_local ~p~n",[{?MODULE,?LINE}]),
    io:format("cookie ~p~n",[{erlang:get_cookie(),?MODULE,?LINE}]),
    ok=local:start_initial(ClusterSpec),
    io:format("OK: local:start_initial ~p~n",[{?MODULE,?LINE}]),
   
    WhichApplications2=[{Node,rpc:call(Node,application,which_applications,[],5000)}||Node<-nodes()],
    io:format("WhichApplications2 !!! ~p~n",[{WhichApplications2,?MODULE,?FUNCTION_NAME,?LINE}]),
    
%    loop(),
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    timer:sleep(2000),
   init:stop(),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
loop()->
    io:format(" ~n"),
    io:format("************************************************** ~n"),
    io:format(" ~n"),
    timer:sleep(20*1000),
    R_parent_stopped=sd:call(infra_service,parent_server,stopped_nodes,[],10000),
    R_pod_stopped=sd:call(infra_service,pod_server,stopped_nodes,[],10000),
    R_appl_stopped=sd:call(infra_service,appl_server,stopped_appls,[],10000),
    
    io:format("R_parent_stopped ~p~n",[{date(),time(),R_parent_stopped,?MODULE,?FUNCTION_NAME}]),
    io:format("R_pod_stopped  ~p~n",[{date(),time(),R_pod_stopped,?MODULE,?FUNCTION_NAME}]),
    io:format("R_appl_stopped  ~p~n",[{date(),time(),R_appl_stopped,?MODULE,?FUNCTION_NAME}]),

    loop().

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% -------------------------------------------------------------------
running_apps(Node)->
    Nodes=lists:delete(node(),[Node|rpc:call(Node,erlang,nodes,[],5000)]),
    [{N,rpc:call(N,application,which_applications,[],5000)}||N<-Nodes].

running_nodes(Node)->
    
    lists:delete(node(),[Node|rpc:call(Node,erlang,nodes,[],5000)]).

%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------

setup(_ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    
    
    ok.
