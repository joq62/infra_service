-define(TABLE,pod_instance).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 cluster_spec_id,
		 cluster_instance,
		 status,
		 pod_name,
		 pod_node,
		 pod_dir,
		 host_spec_id,
		 appl_spec_id
		}).


