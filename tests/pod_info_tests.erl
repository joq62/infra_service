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
-module(pod_info_tests).      
 
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
    ok=init_state(),
     
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.



%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
init_state()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    

    DeplId1=depl_id_1,
    ApplDeployId1="math",
    Status1=candidate,
    PodName1=pod_name1,
    PodNode1=pod_node1,
    PodDir1=pod_dir1,
    ClusterApplDeplId1="a",
    ClusterDeplyomentId1="many_c100_c200",
    HostSpecId1="c100",
    ApplSpecId1="math",
    {atomic,ok}=db_pod_info:create(DeplId1,ApplDeployId1,Status1,
				   PodName1,PodNode1,PodDir1,
				   ClusterApplDeplId1,ClusterDeplyomentId1,
				   HostSpecId1,ApplSpecId1),
    
    

    DeplId2=depl_id_2,
    ApplDeployId2="math",
    Status2=candidate,
    PodName2=pod_name2,
    PodNode2=pod_node2,
    PodDir2=pod_dir2,
    ClusterApplDeplId2="a",
    ClusterDeplyomentId2="many_c100_c200",
    HostSpecId2="c200",
    ApplSpecId2="ops_node",
    {atomic,ok}=db_pod_info:create(DeplId2,ApplDeployId2,Status2,
				   PodName2,PodNode2,PodDir2,
				   ClusterApplDeplId2,ClusterDeplyomentId2,
				   HostSpecId2,ApplSpecId2),
    
    {depl_id_1,"math",candidate,pod_name1,pod_node1,pod_dir1,
     "a","many_c100_c200","c100","math"
    }=db_pod_info:read(DeplId1),
    
    {depl_id_2,"math",candidate,pod_name2,pod_node2,pod_dir2,
     "a","many_c100_c200","c200","ops_node"
    }=db_pod_info:read(DeplId2),
    
    {ok, pod_name1}=db_pod_info:read(pod_name,DeplId1),
    {ok, pod_name2}=db_pod_info:read(pod_name,DeplId2),
    
    {ok,candidate}=db_pod_info:read(status,DeplId1),
    {ok,candidate}=db_pod_info:read(status,DeplId2),

    {atomic,ok}=db_pod_info:add_info(status,deployed,DeplId1),
    {atomic,ok}=db_pod_info:add_info(status,deployed,DeplId2),

    {ok,deployed}=db_pod_info:read(status,DeplId1),
    {ok,deployed}=db_pod_info:read(status,DeplId2),

  

    
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


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
       
    pong=db_etcd:ping(),
    ok=db_pod_info:create_table(),
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.
