-define(TABLE,appl_instance).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 cluster_instance,
		 appl_spec,
		 pod_node,
		 host_spec,
		 status
		}).


