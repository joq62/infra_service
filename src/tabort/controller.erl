%% Author: uabjle
%% Created: 10 dec 2012
%% Description: TODO: Add description to application_org
-module(controller).
 
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-define(SERVER,ops_node_server).
%% --------------------------------------------------------------------
%% Internal exports
%% --------------------------------------------------------------------
-export([
	 appl_name/1,
	 app/1,
	 gitpath/1,
	 hostname/1,
	 worker_host_specs/1,
	 application_spec/1,
	 application_deployment_info/1,
	 cluster_application_deployments/2,
	 connect_nodes_info/0,
	 cluster_deployment_spec/0,
	 get_state/0,
	 ping/0

	]).


%% --------------------------------------------------------------------
%% Macros
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% Records
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------
%% API Functions
%% --------------------------------------------------------------------
appl_name(ApplSpec)->
    gen_server:call(ops_node_server, {appl_name,ApplSpec}).
app(ApplSpec)->
    gen_server:call(ops_node_server, {app,ApplSpec}).
gitpath(ApplSpec)->
    gen_server:call(ops_node_server, {gitpath,ApplSpec}).


worker_host_specs(ClusterDeploymentSpec)->
    gen_server:call(ops_node_server,{worker_host_specs,ClusterDeploymentSpec}).

hostname(HostSpec)->
    gen_server:call(ops_node_server, {hostname,HostSpec}).
cluster_deployment_spec() ->
    gen_server:call(ops_node_server, {cluster_deployment_spec}).

application_deployment_info(ApplDeploymentSpec)->
    gen_server:call(ops_node_server,{application_deployment_info,ApplDeploymentSpec}).

application_spec(ApplicationSpec)->
    gen_server:call(ops_node_server,{application_spec,ApplicationSpec}).

cluster_application_deployments(cluster_spec,ClusterSpec)->    
    gen_server:call(ops_node_server, {cluster_application_deployments,
				      cluster_spec,ClusterSpec});

cluster_application_deployments(appl_deployment_specs,ClusterApplDeployment)->    
    gen_server:call(ops_node_server, {cluster_application_deployments,
				      appl_deployment_specs,ClusterApplDeployment}).
connect_nodes_info()->
    gen_server:call(ops_cluster_controller_server,{connect_nodes_info}).

ping() ->
    gen_server:call(ops_node_server, {ping}).

get_state() ->
    gen_server:call(ops_node_server, {get_state}).

%% ====================================================================!
%% External functions
%% ====================================================================!


%% ====================================================================
%% Internal functions
%% ====================================================================
