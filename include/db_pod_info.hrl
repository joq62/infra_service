-define(TABLE,pod_info).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 deploy_id,
		 appl_deployment_id,
		 status,
		 pod_name,
		 pod_node,
		 pod_dir,
		 cluster_application_deployment_id,
		 cluster_deployment_id,
		 host_spec_id,
		 appl_spec_id
		}).


