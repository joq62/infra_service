-define(ClusterSpecDir,"cluster_specs").
-define(GitPathClusterSpecs,"https://github.com/joq62/cluster_specs.git").

-define(TABLE,cluster_spec).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 spec_id,
		 cookie,
		 dir,
		 num_controllers,
		 controller_host_specs,
		 num_workers,
		 worker_host_specs
		}).
