-define(TABLE,application_instance).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 instance_id,
		 cluster_spec,
		 appl_spec,
		 app,
		 pod_node,
		 host_spec,
		 status
		}).


