{application,db_etcd,
             [{description,"An OTP library"},
              {vsn,"0.1.0"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {env,[]},
              {modules,[db_appl_deployment,db_appl_instance,db_appl_spec,
                        db_appl_state,db_cluster_application_deployment,
                        db_cluster_instance,db_cluster_spec,db_etcd,
                        db_etcd_server,db_host_spec,db_pod_info,lib_db_etcd]},
              {licenses,["Apache 2.0"]},
              {links,[]}]}.