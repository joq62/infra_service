-define(HostSpecDir,"host_specs").
-define(GitPathHostSpecs,"https://github.com/joq62/host_specs.git").

-define(TABLE,host_spec).
-define(RECORD,?TABLE).
-record(?RECORD,{
		 spec_id,
		 hostname,
		 local_ip,
		 ssh_port,
		 uid,
		 passwd,
		 application_config
		}).
