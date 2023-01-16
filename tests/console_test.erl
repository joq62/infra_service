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
 %   ok=secure_parents_pods_started(),
 %   ok=install_appls(),
    ok=orchistrate(),
    
        
  
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
orchistrate()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    io:format("Stopped Parents ~p~n",[{parent_server:stopped_nodes(),?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("Stopped Pods ~p~n",[{pod_server:stopped_nodes(),?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("Stopped Appls ~p~n",[{appl_server:stopped_appls(),?MODULE,?FUNCTION_NAME,?LINE}]),

    

    % 1). check and restart stopped parents
    {ok,StoppedParents}=parent_server:stopped_nodes(),
    [parent_server:create_node(Parent)||Parent<-StoppedParents],
    {ok,ActiveParents}=parent_server:active_nodes(),
    [{net_adm:ping(Pod1),rpc:call(Pod1,net_adm,ping,[Pod2],5000)}||Pod1<-ActiveParents,
								   Pod2<-ActiveParents,
								   Pod1/=Pod2],
    % 2). check and restart stopped pods
    {ok,StoppedPods}=pod_server:stopped_nodes(),
    [create_pod(Pod)||Pod<-StoppedPods],
    [rpc:call(Pod1,net_adm,ping,[Pod2],5000)||Pod1<-ActiveParents,
					      Pod2<-StoppedPods,
					      Pod1/=Pod2],
    % 3). check and restart stopped appls
    {ok,StoppedApplInfoLists}=appl_server:stopped_appls(),
    StoppedCommon=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					   common==App],
    [{error,Reason}||{error,Reason}<-create_appl(StoppedCommon,[])],
    StoppedResourceDiscovery=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					   resource_discovery==App],
    [{error,Reason}||{error,Reason}<-create_appl(StoppedResourceDiscovery,[])],
    StoppedUserApplications=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
						     common/=App,
						     resource_discovery/=App,
						     db_etcd/=App,
						     nodelog/=App,
						     infra_service/=App],
    [{error,Reason}||{error,Reason}<-create_appl(StoppedUserApplications,[])],

    % Radnomly kill a node or parent 


    % Wait 30 seconds to redo
    timer:sleep(20*1000),
    orchistrate().
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
install_appls()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    {ok,StoppedApplInfoLists}=appl_server:stopped_appls(),
    {ok,[]}=appl_server:active_appls(),

    %-- StoppedApplInfo={PodNode,ApplSpec,App}
    % Load and Start common 
    StoppedCommon=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					   common==App],
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedCommon,[])],
    % Load and Start resource_discovery
    StoppedResourceDiscovery=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
					   resource_discovery==App],
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedResourceDiscovery,[])],

    % Load and Start nodelog  
    % Load and Start db_etcd 
    % Load and star infra_service

    % Load and start applications

    StoppedUserApplications=[{PodNode,ApplSpec,App}||{PodNode,ApplSpec,App}<-StoppedApplInfoLists,
						     common/=App,
						     resource_discovery/=App,
						     db_etcd/=App,
						     nodelog/=App,
						     infra_service/=App],
    
    []=[{error,Reason}||{error,Reason}<-create_appl(StoppedUserApplications,[])],

    ok.

create_appl([],Acc)->
    Acc;
create_appl([{PodNode,ApplSpec,App}|T],Acc)->
    Result=appl_server:create_appl(ApplSpec,PodNode),
    io:format("Ping  ~p~n",[{rpc:call(PodNode,App,ping,[],2000),PodNode,ApplSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    io:format("Creat Appl Result ~p~n",[{Result,PodNode,ApplSpec,?MODULE,?FUNCTION_NAME,?LINE}]),
    create_appl(T,[{Result,PodNode,ApplSpec,App}|Acc]).
    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
secure_parents_pods_started()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
      
    [rpc:call(Pod,init,stop,[],5000)||Pod<-db_parent_desired_state:get_all_id()],
    timer:sleep(2000),
    [rpc:call(Pod,init,stop,[],5000)||Pod<-db_pod_desired_state:get_all_id()],
    timer:sleep(1000),
    
    %------
    
    
    {ok,StoppedParents}=parent_server:stopped_nodes(),
    [ok,ok]=[parent_server:create_node(Parent)||Parent<-StoppedParents],
    {ok,ActiveParents}=parent_server:active_nodes(),
    [{pong,pong},{pong,pong}]=[{net_adm:ping(Pod1),rpc:call(Pod1,net_adm,ping,[Pod2],5000)}||Pod1<-ActiveParents,
							  Pod2<-ActiveParents,
							  Pod1/=Pod2],
    {ok,[_,_]}=parent_server:active_nodes(),
    {ok,[]}=parent_server:stopped_nodes(),
    
    {ok,StoppedPods}=pod_server:stopped_nodes(),
    [ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok]=[create_pod(Pod)||Pod<-StoppedPods],
    [rpc:call(Pod1,net_adm,ping,[Pod2],5000)||Pod1<-ActiveParents,
					      Pod2<-StoppedPods,
					      Pod1/=Pod2],
    
    {ok,ActivePods}=pod_server:active_nodes(),
    [
     '1_c200_c201_pod@c200','1_c200_c201_pod@c201',
     '2_c200_c201_pod@c200','2_c200_c201_pod@c201',
     '3_c200_c201_pod@c200','3_c200_c201_pod@c201',
     '4_c200_c201_pod@c200','4_c200_c201_pod@c201',
     '5_c200_c201_pod@c200','5_c200_c201_pod@c201',
     '6_c200_c201_pod@c200','6_c200_c201_pod@c201'
    ]=lists:sort(ActivePods),
    {ok,[]}=pod_server:stopped_nodes(),

    io:format("nodes ~p~n",[{nodes(),?MODULE,?FUNCTION_NAME}]),
    
    ok.
create_pod(PodNode)->
    {ok,ParentNode}=db_pod_desired_state:read(parent_node,PodNode),
    {ok,NodeName}=db_pod_desired_state:read(node_name,PodNode),
    {ok,PodDir}=db_pod_desired_state:read(pod_dir,PodNode),
    {ok,PaArgsList}=db_pod_desired_state:read(pa_args_list,PodNode),
    {ok,EnvArgs}=db_pod_desired_state:read(env_args,PodNode),
    pod_server:create_pod(ParentNode,NodeName,PodDir,PaArgsList,EnvArgs).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
check_appl_status()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    [rpc:call(Pod,init,stop,[],5000)||Pod<-db_parent_desired_state:get_all_id()],
    timer:sleep(2000),
    [rpc:call(Pod,init,stop,[],5000)||Pod<-db_pod_desired_state:get_all_id()],
    timer:sleep(1000),


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
-define(LocalTypes,[oam,nodelog,db_etcd]).
-define(TargetTypes,[oam,nodelog,db_etcd]).

setup()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    {ok,_}=console_server:start(),
    pong=console_server:ping(),
    
    ok.
