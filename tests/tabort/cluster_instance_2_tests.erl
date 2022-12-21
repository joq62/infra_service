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
-module(cluster_instance_2_tests).      
 
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
   
    ok=db_cluster_instance:create_table(),
    
    

    ClusterSpec="c200_c201",
    Type1=controller,
    PodName1=pod_name_1,
    PodNode1=pod_node_1,
    PodDir1=pod_dir_1,
    HostSpec1="c200",
    Status1=candidate,

    []=db_cluster_instance:nodes(Type1,ClusterSpec),

    {atomic,ok}=db_cluster_instance:create(ClusterSpec,Type1,PodName1,PodNode1,PodDir1,HostSpec1,Status1),
    
    [{"c200_c201",controller,pod_name_1,pod_node_1,pod_dir_1,"c200",candidate}]=db_cluster_instance:read(ClusterSpec),
 
    Type2=connect,
    PodName2=pod_name_2,
    PodNode2=pod_node_2,
    PodDir2=pod_dir_2,
    HostSpec2="c201",
    Status2=deployed,

    {atomic,ok}=db_cluster_instance:create(ClusterSpec,Type2,PodName2,PodNode2,PodDir2,HostSpec2,Status2),
  
    [
     {"c200_c201",controller,pod_name_1,pod_node_1,pod_dir_1,"c200",candidate},
     {"c200_c201",connect,pod_name_2,pod_node_2,pod_dir_2,"c201",deployed}
    ]=db_cluster_instance:read(ClusterSpec),
    
    

    {ok,pod_name_1}=db_cluster_instance:read(pod_name,ClusterSpec,PodNode1),
    {ok,pod_name_2}=db_cluster_instance:read(pod_name,ClusterSpec,PodNode2),
     {ok,"c200_c201"}=db_cluster_instance:read(cluster_spec,ClusterSpec,PodNode1),
    {ok,controller}=db_cluster_instance:read(type,ClusterSpec,PodNode1),
    {ok,pod_node_1 }=db_cluster_instance:read(pod_node,ClusterSpec,PodNode1),
    {ok,pod_dir_1}=db_cluster_instance:read(pod_dir,ClusterSpec,PodNode1),

    {ok,"c200"}=db_cluster_instance:read(host_spec,ClusterSpec,PodNode1),
    {ok,deployed}=db_cluster_instance:read(status,ClusterSpec,PodNode2),
  
    [pod_node_1]=db_cluster_instance:nodes(Type1,ClusterSpec),
  
    []=db_cluster_instance:read(status,ClusterSpec,glurk),
    {error,['Key eexists',glurk,"c200_c201",db_cluster_instance,_]}=db_cluster_instance:read(glurk,ClusterSpec,PodNode1),
 
      
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
