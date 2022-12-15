-define(ApplSpecDir,"application_specs").
-define(GitPathApplSpecs,"https://github.com/joq62/application_specs.git").

-define(TABLE,application_spec).
-define(RECORD,application_spec).
-record(?RECORD,{
		 spec_id,
		 appl_name,
		 vsn,
		 app,
		 gitpath,
		 local_resource_type,
		 target_resource_type
		}).
