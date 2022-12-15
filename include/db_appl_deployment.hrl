-define(ApplDeploymentDir,"application_deployments").
-define(GitPathApplDeployments,"https://github.com/joq62/application_deployments.git").

-define(TABLE,appl_deployment).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 spec_id,
		 appl_spec,
		 vsn,
		 cluster_spec,
		 num_instances,
		 affinity
		}).
