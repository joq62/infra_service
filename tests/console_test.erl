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
-module(console_test).      
 
-export([start/1]).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
start([ClusterSpec,_Arg2])->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok=setup(),
    ok=initiate_test(ClusterSpec),
    ok=desired_test(),
    ok=check_appl_status(),

    ok=create_parents(),
        
  
   %ok=create_connect(ClusterSpec,StartHostSpec),
%    ok=init_test(ClusterSpec,StartHostSpec),
        
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),
  %  init:stop(),
  %  timer:sleep(2000),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
create_parents()->
    glurk=parent_server:stopped_nodes(),
    
    

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
check_appl_status()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {ok,StoppedApplsInfo}=lib_appl:stopped_appls(),
    [
     {'1_c200_c201_pod@c200',"common",common},
     {'1_c200_c201_pod@c200',"resource_discovery",resource_discovery},
     {'1_c200_c201_pod@c201',"common",common},
     {'1_c200_c201_pod@c201',"resource_discovery",resource_discovery},
     {'2_c200_c201_pod@c200',"common",common},
     {'2_c200_c201_pod@c200',"resource_discovery",resource_discovery},
     {'2_c200_c201_pod@c201',"common",common},
     {'2_c200_c201_pod@c201',"resource_discovery",resource_discovery},
     {'3_c200_c201_pod@c200',"common",common},
     {'3_c200_c201_pod@c200',"math",math},
     {'3_c200_c201_pod@c200',"resource_discovery",resource_discovery},
     {'3_c200_c201_pod@c201',"common",common},
     {'3_c200_c201_pod@c201',"math",math},
     {'3_c200_c201_pod@c201',"resource_discovery",resource_discovery},
     {'4_c200_c201_pod@c200',"common",common},
     {'4_c200_c201_pod@c200',"infra_service",infra_service},
     {'4_c200_c201_pod@c200',"resource_discovery",resource_discovery},
     {'4_c200_c201_pod@c201',"common",common},
     {'4_c200_c201_pod@c201',"hw_conbee",hw_conbee_app},
     {'4_c200_c201_pod@c201',"resource_discovery",resource_discovery},
     {'5_c200_c201_pod@c200',"common",common},
     {'5_c200_c201_pod@c200',"resource_discovery",resource_discovery},
     {'5_c200_c201_pod@c201',"common",common},
     {'5_c200_c201_pod@c201',"resource_discovery",resource_discovery},
     {'6_c200_c201_pod@c200',"common",common},
     {'6_c200_c201_pod@c200',"resource_discovery",resource_discovery},
     {'6_c200_c201_pod@c201',"common",common},
     {'6_c200_c201_pod@c201',"resource_discovery",resource_discovery}
    ]=lists:sort(StoppedApplsInfo),

    {ok,[]}=lib_appl:active_appls(),

    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
desired_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    [
     {'c200_c201_parent@c201',"c200_c201_parent","c200_c201","c201"," -pa c200_c201 "," -pa c200_c201/*/ebin"," "},
     {'c200_c201_parent@c200',"c200_c201_parent","c200_c201","c200"," -pa c200_c201 "," -pa c200_c201/*/ebin"," "}
    ]=db_parent_desired_state:read_all(),
    
    
    [
     {'1_c200_c201_pod@c200',"1_c200_c201_pod","c200_c201/1_c200_c201_pod",'c200_c201_parent@c200',["common","resource_discovery"],"c200_c201","c200",[]," "},
     {'1_c200_c201_pod@c201',"1_c200_c201_pod","c200_c201/1_c200_c201_pod",'c200_c201_parent@c201',["common","resource_discovery"],"c200_c201","c201",[]," "},
     {'2_c200_c201_pod@c200',"2_c200_c201_pod","c200_c201/2_c200_c201_pod",'c200_c201_parent@c200',["common","resource_discovery"],"c200_c201","c200",[]," "},
     {'2_c200_c201_pod@c201',"2_c200_c201_pod","c200_c201/2_c200_c201_pod",'c200_c201_parent@c201',["common","resource_discovery"],"c200_c201","c201",[]," "},
     {'3_c200_c201_pod@c200',"3_c200_c201_pod","c200_c201/3_c200_c201_pod",'c200_c201_parent@c200',["common","resource_discovery","math"],"c200_c201","c200",[]," "},
     {'3_c200_c201_pod@c201',"3_c200_c201_pod","c200_c201/3_c200_c201_pod",'c200_c201_parent@c201',["common","resource_discovery","math"],"c200_c201","c201",[]," "},
     {'4_c200_c201_pod@c200',"4_c200_c201_pod","c200_c201/4_c200_c201_pod",'c200_c201_parent@c200',["common","infra_service","resource_discovery"],"c200_c201","c200",[]," "},
     {'4_c200_c201_pod@c201',"4_c200_c201_pod","c200_c201/4_c200_c201_pod",'c200_c201_parent@c201',["hw_conbee","common","resource_discovery"],"c200_c201","c201",[]," "},
     {'5_c200_c201_pod@c200',"5_c200_c201_pod","c200_c201/5_c200_c201_pod",'c200_c201_parent@c200',["common","resource_discovery"],"c200_c201","c200",[]," "},{'5_c200_c201_pod@c201',"5_c200_c201_pod","c200_c201/5_c200_c201_pod",'c200_c201_parent@c201',["common","resource_discovery"],"c200_c201","c201",[]," "},
     {'6_c200_c201_pod@c200',"6_c200_c201_pod","c200_c201/6_c200_c201_pod",'c200_c201_parent@c200',["common","resource_discovery"],"c200_c201","c200",[]," "},{'6_c200_c201_pod@c201',"6_c200_c201_pod","c200_c201/6_c200_c201_pod",'c200_c201_parent@c201',["common","resource_discovery"],"c200_c201","c201",[]," "}
    ]=lists:sort(db_pod_desired_state:read_all()),

    [
     '1_c200_c201_pod@c200','1_c200_c201_pod@c201','2_c200_c201_pod@c200',
     '2_c200_c201_pod@c201','3_c200_c201_pod@c200','3_c200_c201_pod@c201',
     '4_c200_c201_pod@c200','4_c200_c201_pod@c201','5_c200_c201_pod@c200',
     '5_c200_c201_pod@c201','6_c200_c201_pod@c200','6_c200_c201_pod@c201'
    ]=lists:sort(db_pod_desired_state:get_all_id()),
    
    AllApplsDesiredState=[{PodNode,db_pod_desired_state:read(appl_spec_list,PodNode)}||PodNode<-lists:sort(db_pod_desired_state:get_all_id()),
									     {ok,[]}/=db_pod_desired_state:read(appl_spec_list,PodNode)],
    [
     {'1_c200_c201_pod@c200',{ok,["common","resource_discovery"]}},{'1_c200_c201_pod@c201',{ok,["common","resource_discovery"]}},
     {'2_c200_c201_pod@c200',{ok,["common","resource_discovery"]}},{'2_c200_c201_pod@c201',{ok,["common","resource_discovery"]}},
     {'3_c200_c201_pod@c200',{ok,["common","resource_discovery","math"]}},{'3_c200_c201_pod@c201',{ok,["common","resource_discovery","math"]}},
     {'4_c200_c201_pod@c200',{ok,["common","infra_service","resource_discovery"]}},
     {'4_c200_c201_pod@c201',{ok,["hw_conbee","common","resource_discovery"]}},
     {'5_c200_c201_pod@c200',{ok,["common","resource_discovery"]}},{'5_c200_c201_pod@c201',{ok,["common","resource_discovery"]}},
     {'6_c200_c201_pod@c200',{ok,["common","resource_discovery"]}},{'6_c200_c201_pod@c201',{ok,["common","resource_discovery"]}}
    ]=lists:sort(AllApplsDesiredState),
    
    ok.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
initiate_test(ClusterSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    {error,["Not yet initiated with a cluster spec :"]}=console_server:load_desired_state(parents),
    {error,["Not yet initiated with a cluster spec :"]}=console_server:load_desired_state(pods),
 %   {error,["Not yet initiated with a cluster spec :"]}=console_server:load_desired_state(appls),
    {error,["Not yet initiated with a cluster spec :"]}=console_server:load_desired_state(glurk),
    
    ok=console_server:initiate(ClusterSpec),
    {errror,["Already intitaited : ",ClusterSpec]}=console_server:initiate(ClusterSpec),
    ok=console_server:load_desired_state(parents),
    ok=console_server:load_desired_state(pods),
    ok=console_server:load_desired_state(appls),
    {error,["Type not supported ",glurk]}=console_server:load_desired_state(glurk),
    
    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
create_connect(ClusterSpec,StartHostSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    %% Ensure that config is started proprely , strange!!
    ok=db_config:create_table(),
    {atomic,ok}=db_config:set(cluster_spec,ClusterSpec),
    ClusterSpec=db_config:get(cluster_spec),
    
      % Create first connect node with right cookie
    {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
    erlang:set_cookie(node(),list_to_atom(Cookie)),
    
    % create connect nodes
    {ok,_}=connect_server:create_dbase_info(ClusterSpec), 
    ConnectStart=connect_server:create_connect_nodes(ClusterSpec),
    []=[{error,Reason}||{error,Reason}<-ConnectStart],
    
    % Create connect pod
    
    % Create controller and  worker nodes 

    {Controllers,[]}=pod_server:create_controller_pods(ClusterSpec),
    {Workers,[]}=pod_server:create_worker_pods(ClusterSpec),
   
    % Install basic applications on each node but connect 
    RC1=[appl_server:new_on_pod("common",PodNode,ClusterSpec,10*1000)||PodNode<-Controllers],
    []=[{error,Reason}||{error,Reason}<-RC1],
    []=[{error,PodNode}||PodNode<-Controllers,
			 false==lists:keymember(common,1,rpc:call(PodNode,application,which_applications,[],1000))],

    RC2=[appl_server:new_on_pod("resource_discovery",PodNode,ClusterSpec,10*1000)||PodNode<-Controllers],
    []=[{error,Reason}||{error,Reason}<-RC2],
    []=[{error,PodNode}||PodNode<-Controllers,
			 false==lists:keymember(resource_discovery,1,rpc:call(PodNode,application,which_applications,[],1000))],

    RW1=[appl_server:new_on_pod("common",PodNode,ClusterSpec,10*1000)||PodNode<-Workers],
    []=[{error,Reason}||{error,Reason}<-RW1],
    []=[{error,PodNode}||PodNode<-Workers,
			 false==lists:keymember(common,1,rpc:call(PodNode,application,which_applications,[],1000))],

    RW2=[appl_server:new_on_pod("resource_discovery",PodNode,ClusterSpec,10*1000)||PodNode<-Workers],
    []=[{error,Reason}||{error,Reason}<-RW2],
    []=[{error,PodNode}||PodNode<-Workers,
			 false==lists:keymember(resource_discovery,1,rpc:call(PodNode,application,which_applications,[],1000))],

    
	
    %% Initiate the first controller before hand over to infra_service
    [FirstController|_]=lists:sort(Controllers),
    
    ok=appl_server:new_on_pod("nodelog",FirstController,ClusterSpec,10*1000), 
    ok=appl_server:new_on_pod("db_etcd",FirstController,ClusterSpec,10*1000), 
    
  
    % hang up

    
    AllApps=[{PodNode,rpc:call(PodNode,application,which_applications,[],1000)}||PodNode<-lists:append(Controllers,Workers)],
    io:format("AllApps ~p~n",[{AllApps,?MODULE,?FUNCTION_NAME}]),

    

    % Kill install node
    

    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
init_test(ClusterSpec,StartHostSpec)->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    %% Ensure that config is started proprely , strange!!
    ok=db_config:create_table(),
    {atomic,ok}=db_config:set(cluster_spec,ClusterSpec),
    ClusterSpec=db_config:get(cluster_spec),
    
      % Create first connect node with right cookie
    {ok,Cookie}=db_cluster_spec:read(cookie,ClusterSpec),
    erlang:set_cookie(node(),list_to_atom(Cookie)),
    
    % create connect nodes
    {ok,_}=connect_server:create_dbase_info(ClusterSpec), 
    ConnectStart=connect_server:create_connect_nodes(ClusterSpec),
    []=[{error,Reason}||{error,Reason}<-ConnectStart],
    
    % Create connect pod
    
    % Create controller and  worker nodes 

    {Controllers,[]}=pod_server:create_controller_pods(ClusterSpec),
    {Workers,[]}=pod_server:create_worker_pods(ClusterSpec),
   
    % Install basic applications on each node but connect 
    RC1=[appl_server:new_on_pod("common",PodNode,ClusterSpec,10*1000)||PodNode<-Controllers],
    []=[{error,Reason}||{error,Reason}<-RC1],
    []=[{error,PodNode}||PodNode<-Controllers,
			 false==lists:keymember(common,1,rpc:call(PodNode,application,which_applications,[],1000))],

    RC2=[appl_server:new_on_pod("resource_discovery",PodNode,ClusterSpec,10*1000)||PodNode<-Controllers],
    []=[{error,Reason}||{error,Reason}<-RC2],
    []=[{error,PodNode}||PodNode<-Controllers,
			 false==lists:keymember(resource_discovery,1,rpc:call(PodNode,application,which_applications,[],1000))],

    RW1=[appl_server:new_on_pod("common",PodNode,ClusterSpec,10*1000)||PodNode<-Workers],
    []=[{error,Reason}||{error,Reason}<-RW1],
    []=[{error,PodNode}||PodNode<-Workers,
			 false==lists:keymember(common,1,rpc:call(PodNode,application,which_applications,[],1000))],

    RW2=[appl_server:new_on_pod("resource_discovery",PodNode,ClusterSpec,10*1000)||PodNode<-Workers],
    []=[{error,Reason}||{error,Reason}<-RW2],
    []=[{error,PodNode}||PodNode<-Workers,
			 false==lists:keymember(resource_discovery,1,rpc:call(PodNode,application,which_applications,[],1000))],

    
	
    %% Initiate the first controller before hand over to infra_service
    [FirstController|_]=lists:sort(Controllers),
    
    ok=appl_server:new_on_pod("nodelog",FirstController,ClusterSpec,10*1000), 
    ok=appl_server:new_on_pod("db_etcd",FirstController,ClusterSpec,10*1000), 
    
  
    % hang up

    
    AllApps=[{PodNode,rpc:call(PodNode,application,which_applications,[],1000)}||PodNode<-lists:append(Controllers,Workers)],
    io:format("AllApps ~p~n",[{AllApps,?MODULE,?FUNCTION_NAME}]),

    

    % Kill install node
    

    ok.
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
-define(LocalTypes,[oam,nodelog,db_etcd]).
-define(TargetTypes,[oam,nodelog,db_etcd]).

setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    {ok,_}=console_server:start(),
    pong=console_server:ping(),
    
    ok.
