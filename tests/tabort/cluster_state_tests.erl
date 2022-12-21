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
-module(cluster_state_tests).      
 
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
    ok=update_test(),
  
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
    ClusterName1="cl_name_1",
    ControllerPod1={cl_node1,cl_pod_dir_1,host1},
    WorkerPod1={w_node1,w_pod_dir_1,host1},
    DeployInfo1={date1,time1},
    {atomic,ok}=db_cluster_state:create(DeplId1,ClusterName1,ControllerPod1,WorkerPod1,DeployInfo1),
    
    {depl_id_1,
     "cl_name_1",
     [{cl_node1,cl_pod_dir_1,host1}],
     [{w_node1,w_pod_dir_1,host1}],
     [{date1,time1}]
    }=db_cluster_state:read(DeplId1),

    DeplId2=depl_id_2,
    ClusterName2="cl_name_2",
    ControllerPod2={cl_node2,cl_pod_dir_2,host2},
    WorkerPod2={w_node2,w_pod_dir_2,host2},
    DeployInfo2={date2,time2},
    {atomic,ok}=db_cluster_state:create(DeplId2,ClusterName2,ControllerPod2,WorkerPod2,DeployInfo2),
    
    {depl_id_2,
     "cl_name_2",
     [{cl_node2,cl_pod_dir_2,host2}],
     [{w_node2,w_pod_dir_2,host2}],
     [{date2,time2}]
    }=db_cluster_state:read(DeplId2),
    
    {ok,"cl_name_2"}=db_cluster_state:read(cluster_name,DeplId2),
    {ok,[{cl_node2,cl_pod_dir_2,host2}]}=db_cluster_state:read(controller_pods,DeplId2),
    {ok, [{w_node2,w_pod_dir_2,host2}]}=db_cluster_state:read(worker_pods,DeplId2),
    {ok,[{date2,time2}]}=db_cluster_state:read(deployment_info,DeplId2),
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
update_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  
    [DepId1|_]=lists:sort(db_cluster_state:get_all_id()),
    depl_id_1=DepId1,
    {ok,"cl_name_1"}=db_cluster_state:read(cluster_name,DepId1),
    {ok,ControllerPods1}=db_cluster_state:read(controller_pods,DepId1),
    % add
    
    AddControllerPods={cl_node12,cl_pod_dir_12,host2},
       
    {atomic,ok}=db_cluster_state:add_info(controller_pods,AddControllerPods,DepId1),
    {ok,ControllerPods2}=db_cluster_state:read(controller_pods,DepId1),
    
   [{cl_node1,cl_pod_dir_1,host1},
    {cl_node12,cl_pod_dir_12,host2}
   ]=lists:sort(ControllerPods2),
    
    {atomic,ok}=db_cluster_state:delete_info(controller_pods,glurk,DepId1),
    {ok,ControllerPods2}=db_cluster_state:read(controller_pods,DepId1),

    {atomic,ok}=db_cluster_state:delete_info(controller_pods,AddControllerPods,DepId1),
    {ok,ControllerPods1}=db_cluster_state:read(controller_pods,DepId1),

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


setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
       
    pong=db_etcd:ping(),
    ok=db_cluster_state:create_table(),
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.
