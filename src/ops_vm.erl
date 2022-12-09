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
-module(ops_vm).   
 
-export([
	 ssh_create/7,
	 ssh_create/8,
	 ssh_delete/3,
	 create/5,
	 delete/3,
	 
	 present/1
	]).
		 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
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
    Result=case rpc:call(ControllerNode,slave,start,[HostName,PodNodeName,Args],?TimeOut) of
	       {badrpc,Reason}->
		   {error,[badrpc,Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	       {error,Reason}->
		   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
	       {ok,PodNode}->
		   case rpc:call(PodNode,file,make_dir,[PodDir],5000) of
		       {error,Reason}->
			   {error,[Reason,?MODULE,?FUNCTION_NAME,?LINE]};
		       ok-> 
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
 
