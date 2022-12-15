-define(ClusterApplDeploymentDir,"cluster_application_deployments").
-define(GitPathClusterApplDeployments,"https://github.com/joq62/cluster_application_deployments.git").

-define(TABLE,cluster_application_deployment).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 spec_id,
		 cluster_spec,
		 appl_deployment_specs
		}).
