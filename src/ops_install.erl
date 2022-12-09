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
-module(ops_install).   

-define(TimeOut,7000).
 
-export([
	 start/0,
	 install/3
	]).
		 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% -------------------------------------------------------------------
start()->
    {ok,_}=db_etcd_server:start(),
    {ok,_}=common:start(),
    pong=db_etcd:ping(),
    pong=common:ping(),
    db_etcd:install(),
    install("many_c100_c200","c100","ops_node"),
    ok.
    

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% -------------------------------------------------------------------

install(ClusterDeployment,StartHostSpec,ApplSpec)->    
    %%  % Create the connect node
    {ok,Cookie}=db_cluster_deployment:read(cookie,ClusterDeployment),
    erlang:set_cookie(node(),list_to_atom(Cookie)),
    {ok,ClusterDir}=db_cluster_deployment:read(dir,ClusterDeployment),
    {ok,ControllerHostSpecs}=db_cluster_deployment:read(controller_host_specs,ClusterDeployment),
    [{ok,HostName}]=[db_host_spec:read(hostname,HostSpec)||HostSpec<-ControllerHostSpecs,
							   StartHostSpec=:=HostSpec],
    ConnectNodeName=ClusterDeployment++"_"++"connect_node",    
    NodesToConnect=[node()],
    ConnectNode=list_to_atom(ConnectNodeName++"@"++HostName),
    rpc:call(ConnectNode,init,stop,[]),
    timer:sleep(3000),
    PaArgs=" -pa "++ClusterDir,
    EnvArgs=" -detached ",
    ops_vm:ssh_create(HostName,ConnectNodeName,ClusterDir,Cookie,PaArgs,EnvArgs,NodesToConnect,?TimeOut),
    pong=net_adm:ping(ConnectNode),
    
    % clean up from previous
   
    % Create the pod
    PodNodeName=erlang:integer_to_list(os:system_time(microsecond),36)++"_pod",
    PodDir=PodNodeName++".dir",
    PodDirPath=filename:join(ClusterDir,PodDir),
    ok=rpc:call(ConnectNode,file,make_dir,[PodDirPath],5000),
    {ok,PodNode,PodDir}=ops_vm:create(HostName,ConnectNode,PodNodeName,PodDir,Cookie),

    % load and start ops_node
    {ok,GitPath}=db_appl_spec:read(gitpath,ApplSpec),
    {ok,AppId}=db_appl_spec:read(appl_name,ApplSpec),
    {ok,App}=db_appl_spec:read(app,ApplSpec),

    DirToClone=filename:join([ClusterDir,PodDir,AppId]),  
    ok=rpc:call(PodNode,file,make_dir,[DirToClone],5000),
    {ok,_CloneDir}=appl:git_clone_to_dir(PodNode,GitPath,DirToClone),
    Paths=[filename:join(DirToClone,"ebin")],
    ok=appl:load(PodNode,App,Paths),
    AppEnv=[{App,[{cluster_deployment,ClusterDeployment}]}],
    ok=rpc:call(PodNode,application,set_env,[AppEnv],5000), 
    ok=rpc:call(PodNode,application,start,[App],10000),
   
    ok.
    


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% -------------------------------------------------------------------
