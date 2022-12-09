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
-module(host_spec_2_tests).      
 
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
    ok=read_specs_test(),
  
    io:format("Stop OK !!! ~p~n",[{?MODULE,?FUNCTION_NAME}]),

    ok.


%% --------------------------------------------------------------------
%% Function: available_hosts()
%% Description: Based on hosts.config file checks which hosts are avaible
%% Returns: List({HostId,Ip,SshPort,Uid,Pwd}
%% --------------------------------------------------------------------
read_specs_test()->
    io:format("Start ~p~n",[{?MODULE,?FUNCTION_NAME}]),
    
    ["c100","c200","c201","c202","c300"]=lists:sort(db_host_spec:get_all_id()),

    {"c200","c200","192.168.1.200",22,"joq62","festum01",[]}=db_host_spec:read("c200"),
    
    {ok,"c200"}=db_host_spec:read(hostname,"c200"),
    {ok,"192.168.1.200"}=db_host_spec:read(local_ip,"c200"),
    {ok,22}=db_host_spec:read(ssh_port,"c200"),
    {ok,"joq62"}=db_host_spec:read(uid,"c200"),
    {ok,"festum01"}=db_host_spec:read(passwd,"c200"),
    {ok,[]}=db_host_spec:read(application_config,"c200"),
    


    {error,[eexist,"glurk",db_host_spec,_]}=db_host_spec:read(ssh_port,"glurk"),
    {error,['Key eexists',glurk,"c200",db_host_spec,_]}=db_host_spec:read(glurk,"c200"),
 
    {"c201","c201","192.168.1.201",22,"joq62","festum01",
     [{conbee,[{conbee_addr,"172.17.0.2"},
	       {conbee_port,80},
	       {conbee_key,"D83FA13F74"}
	      ]
      }
     ]
    }=db_host_spec:read("c201"),
    
    
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
