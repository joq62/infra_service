{application,infra_service,
             [{description,"An OTP application"},
              {vsn,"0.1.0"},
              {registered,[]},
              {mod,{infra_service_app,[]}},
              {applications,[kernel,stdlib]},
              {env,[]},
              {modules,[appl,appl_server,controller_server,infra_service,
                        infra_service_app,infra_service_server,
                        infra_service_sup,lib_controller,lib_parent,lib_pod,
                        oam,oam_server,ops_ssh,ops_vm,parent_server,
                        pod_server]},
              {licenses,["Apache-2.0"]},
              {links,[]}]}.