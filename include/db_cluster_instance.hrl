-define(TABLE,cluster_instance).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 cluster_spec,
		 type,
		 pod_name,
		 pod_node,
		 pod_dir,
		 host_spec,
		 status
		}).


