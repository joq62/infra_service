{application,controller,
             [{description,"An OTP application"},
              {vsn,"0.1.0"},
              {registered,[]},
              {mod,{controller_app,[]}},
              {applications,[kernel,stdlib]},
              {env,[]},
              {modules,[appl_server,connect_server,controller,controller_app,
                        controller_server,controller_sup,oam,oam_server,
                        ops_ssh,ops_vm,pod_server]},
              {licenses,["Apache 2.0"]},
              {links,[]}]}.