-define(TABLE,cluster_state).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 deployment_id,   %u
		 cluster_name,
		 controller_pods, %[{Node,PodDir,HostName}]
		 worker_pods,
		 deployment_info %[{date(),time()}
		}).
