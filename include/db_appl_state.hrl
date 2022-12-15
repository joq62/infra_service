-define(TABLE,appl_state).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 deployment_id,   %u
		 appl_name,
		 pods,           %[{Node,PodDir,HostName}]
		 deployment_info %[{date(),time()}
		}).
