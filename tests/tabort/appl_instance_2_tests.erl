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
-module(appl_instance_2_tests).      
 
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
    ok=create_instance_test(),
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.



%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
create_instance_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
   
    ok=db_appl_instance:create_table(),
        
    ClusterInstance1=cluster_instance_id_1,
    ApplSpec1="appl_spec_1",
    PodNode1=pod_node_1,
    HostSpec1=host_spec_1,
    Status1=candidate,

    {atomic,ok}=db_appl_instance:create(ClusterInstance1,ApplSpec1,PodNode1,HostSpec1,Status1),
    
    [{cluster_instance_id_1,"appl_spec_1",pod_node_1,host_spec_1,candidate}]=db_appl_instance:read(ClusterInstance1),
 
    
    ApplSpec2="appl_spec_2",
     PodNode1=pod_node_1,
    Status2=deployed,

    {atomic,ok}=db_appl_instance:create(ClusterInstance1,ApplSpec2,PodNode1,HostSpec1,Status2),
  
   [
    {cluster_instance_id_1,"appl_spec_1",pod_node_1,host_spec_1,candidate},
    {cluster_instance_id_1,"appl_spec_2",pod_node_1,host_spec_1,deployed}
   ]=db_appl_instance:read(cluster_instance_id_1),
    
    

    {ok,["appl_spec_1","appl_spec_2"]}=db_appl_instance:read(appl_spec,cluster_instance_id_1,PodNode1),
    {ok,cluster_instance_id_1}=db_appl_instance:read(cluster_instance,cluster_instance_id_1,PodNode1),
    {ok,host_spec_1}=db_appl_instance:read(host_spec,cluster_instance_id_1,PodNode1),

    {ok,[candidate,deployed]}=db_appl_instance:read(status,cluster_instance_id_1,PodNode1),
  
    []=db_appl_instance:read(status,cluster_instance_id_1,glurk),
    {error,['Key eexists',glurk,cluster_instance_id_1,pod_node_1,db_appl_instance,_]}=db_appl_instance:read(glurk,cluster_instance_id_1,PodNode1),
 
    {atomic,ok}=db_appl_instance:delete(cluster_instance_id_1, ApplSpec2,PodNode1),
    [{cluster_instance_id_1,"appl_spec_1",pod_node_1,host_spec_1,candidate}]=db_appl_instance:read(cluster_instance_id_1),
    
      
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
       
    pong=db_etcd:ping(),
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.
