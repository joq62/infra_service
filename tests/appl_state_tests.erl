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
-module(appl_state_tests).      
 
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
    ApplName1="appl_1",
    PodsInfo1={node1,pod_dir_1,host1},
    DeployInfo1={date1,time1},
    {atomic,ok}=db_appl_state:create(DeplId1,ApplName1,PodsInfo1,DeployInfo1),
    
    {depl_id_1,
     "appl_1",
     [{node1,pod_dir_1,host1}],
     [{date1,time1}]
    }=db_appl_state:read(DeplId1),

    DeplId2=depl_id_2,
    ApplName2="appl_2",
    PodsInfo2={node2,pod_dir_2,host2},
    DeployInfo2={date2,time2},
    {atomic,ok}=db_appl_state:create(DeplId2,ApplName2,PodsInfo2,DeployInfo2),
    
    {
     depl_id_2,
     "appl_2",
     [{node2,pod_dir_2,host2}],
     [{date2,time2}]
    }=db_appl_state:read(DeplId2),
    
    {ok,"appl_2"}=db_appl_state:read(appl_name,DeplId2),
    {ok,[{node2,pod_dir_2,host2}]}=db_appl_state:read(pods,DeplId2),
    {ok,[{date2,time2}]}=db_appl_state:read(deployment_info,DeplId2),
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    ok.

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
update_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  
    Key1=pods,  
    [DepId1|_]=lists:sort(db_appl_state:get_all_id()),
    depl_id_1=DepId1,
    {ok,PodsInfo1}=db_appl_state:read(Key1,DepId1),

    % add
  
    Info1={node11,pod_dir_11,host11},
    
    {atomic,ok}=db_appl_state:add_info(Key1,Info1,DepId1),
    {ok,PodsInfo2}=db_appl_state:read(Key1,DepId1),
    
    [{node1,pod_dir_1,host1},
     {node11,pod_dir_11,host11}
    ]=lists:sort(PodsInfo2),
    
    {atomic,ok}=db_appl_state:delete_info(Key1,glurk,DepId1),
    {ok,PodsInfo2}=db_appl_state:read(Key1,DepId1),

    {atomic,ok}=db_appl_state:delete_info(Key1,Info1,DepId1),
    {ok,PodsInfo1}=db_appl_state:read(Key1,DepId1),

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
    ok=db_appl_state:create_table(),
    
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.
