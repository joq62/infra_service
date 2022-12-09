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
-module(ops_pod).   
 
-export([
	 create/3,
	 create/5,
	 delete/3,
	 load_start/2,
	 load_start/3,
	 ssh_delete/3,
	 ssh_create/7,
	 ssh_create/8,
	 present/1
	]).
		 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(PodAppl,"pod_app").

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% ------------------------------------------------------------------
create(ConnectNode,RootDir,HostSpec)->
    
    PodName=erlang:integer_to_list(os:system_time(microsecond),36)++"_pod",
    PodDir=PodName++".dir",
    FullPathPodDir=filename:join(RootDir,PodDir),
    create(ConnectNode,HostSpec,PodName,FullPathPodDir).
    
create(ConnectNode,HostSpec,PodName,FullPathPodDir)->
    Cookie=atom_to_list(rpc:call(ConnectNode,erlang,get_cookie,[])),
    rpc:call(ConnectNode,os,cmd,["rm -rf "++FullPathPodDir]),
    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
    ops_vm:create(HostName,ConnectNode,PodName,FullPathPodDir,Cookie).

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% ------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% ------------------------------------------------------------------
load_start(PodNode,PodDir)->
    load_start(PodNode,PodDir,?PodAppl).

load_start(PodNode,PodDir,Appl)->
    {ok,App}=db_appl_spec:read(app,Appl),
    {ok,GitPath}=db_appl_spec:read(gitpath,Appl),
    ApplDir=filename:join(PodDir,Appl),
    ApplEbin=filename:join(ApplDir,"ebin"),
    ok=rpc:call(PodNode,file,make_dir,[ApplDir],10000),
    {ok,_}=appl:git_clone_to_dir(PodNode,GitPath,ApplDir),
    ok=appl:load(PodNode,App,[ApplEbin]),
    ok=appl:start(PodNode,App),


    {ok,LocalTypes}=db_appl_spec:read(local_type,Appl),
    {ok,TargetTypes}=db_appl_spec:read(target_type,Appl),
    [rpc:call(PodNode,rd,add_local_resource,[LocalType,PodNode],5000)||LocalType<-LocalTypes],
    [rpc:call(PodNode,rd,add_target_resource_type,[TargetType],5000)||TargetType<-TargetTypes],
    ok=rpc:call(PodNode,rd,trade_resources,[],5000),
    timer:sleep(2000),
  
    {ok,App,ApplDir}.
    
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% -------------------------------------------------------------------
present(Node)->
    case net_adm:ping(Node) of
	pong->
	    true;
	pang->
	    false
    end.



    
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% -------------------------------------------------------------------
ssh_delete(HostName,ConnecNode,NodeDir)->
    ops_ssh:delete_dir(HostName,NodeDir),
    rpc:call(ConnecNode,init,stop,[],2000).
 
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% -------------------------------------------------------------------
-define(TimeOut,10000).

ssh_create(HostName,NodeName,NodeDir,Cookie,PaArgs,EnvArgs,NodesToConnect)->
    ssh_create(HostName,NodeName,NodeDir,Cookie,PaArgs,EnvArgs,NodesToConnect,?TimeOut).
ssh_create(HostName,NodeName,NodeDir,Cookie,PaArgs,EnvArgs,NodesToConnect,TimeOut)->
    %Create erlang vm
    ops_ssh:delete_dir(HostName,NodeDir),
    Result=case ops_ssh:create(HostName,NodeName,Cookie,PaArgs,EnvArgs) of
	       {error,Reason}->
		   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	       {ok,ConnectNode}-> % Create controller and cluster directory
		   case rpc:call(ConnectNode,file,make_dir,[NodeDir],TimeOut) of
		       {badrpc,Reason}->
			   ops_ssh:delete_dir(HostName,NodeDir),
			   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		       ok->
			   PingResult=[{rpc:call(ConnectNode,net_adm,ping,[NodeX],2000),NodeX}||NodeX<-NodesToConnect],
			   {ok,ConnectNode,NodeDir,PingResult}
		   end
	   end,
    Result.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% -------------------------------------------------------------------
delete(ControllerNode,PodeNode,PodDir)->
    rpc:call(ControllerNode,os,cmd,["rm -rf "++PodDir]),
    rpc:call(ControllerNode,slave,stop,[PodeNode],2000),
    ok.
    
    
    
    
%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% -------------------------------------------------------------------
create(HostName,ControllerNode,PodNodeName,PodDir,Cookie)->
    Args=" -setcookie "++Cookie,
    Result=case rpc:call(ControllerNode,slave,start,[HostName,PodNodeName,Args],5000) of
	       {badrpc,Reason}->
		   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	       {error,Reason}->
		   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	       {ok,PodNode}->
		   case rpc:call(PodNode,file,make_dir,[PodDir],5000) of
		       {error,Reason}->
			   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		       ok-> % Git clone controller_app
			   case rpc:call(ControllerNode,net_kernel,connect_node,[PodNode],5000) of
			       {badrpc,Reason}->
				   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
			       false->
				   rpc:call(ControllerNode,os,cmd,["rm -rf "++PodDir],5000),
				   {error,[failed_connect,PodNode]};
			       ignored->
				   {error,[ignored,PodNode,?MODULE,?FUNCTION_NAME,?LINE]};
			       true-> % git clone the Application
				   {ok,PodNode,PodDir}
			   end
		   end
	   end,
    Result.
 
