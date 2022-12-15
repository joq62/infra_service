-module(db_pod_info).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_pod_info.hrl").

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)}
				 ]),
    mnesia:wait_for_tables([?TABLE], 20000).

create_table(NodeList,StorageType)->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {StorageType,NodeList}]),
    mnesia:wait_for_tables([?TABLE], 20000).

add_node(Node,StorageType)->
    Result=case mnesia:change_config(extra_db_nodes, [Node]) of
	       {ok,[Node]}->
		   mnesia:add_table_copy(schema, node(),StorageType),
		   mnesia:add_table_copy(?TABLE, node(), StorageType),
		   Tables=mnesia:system_info(tables),
		   mnesia:wait_for_tables(Tables,20*1000);
	       Reason ->
		   Reason
	   end,
    Result.

create(DeplId,ApplDeployId,Status,
       PodName,PodNode,PodDir,
       ClusterApplicationDeploymentId,ClusterSpecId,
       HostSpecId,
       ApplSpecId)->
 
    Record=#?RECORD{
		    deploy_id=DeplId,
		    appl_deployment_id=ApplDeployId,
		    status=Status,
		    pod_name=PodName,
		    pod_node=PodNode,
		    pod_dir=PodDir,
		    cluster_application_deployment_id=ClusterApplicationDeploymentId,
		    cluster_deployment_id=ClusterSpecId,
		    host_spec_id=HostSpecId,
		    appl_spec_id=ApplSpecId
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

member(DeplId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.deploy_id==DeplId])),
    Member=case Z of
	       []->
		   false;
	       _->
		   true
	   end,
    Member.


read(Key,DeplId)->
    Return=case read(DeplId) of
	       []->
		   {error,[eexist,DeplId,?MODULE,?LINE]};
	       {_DeplId,ApplDeployId,Status,
		PodName,PodNode,PodDir,
		ClusterApplicationDeploymentId,ClusterSpecId,
		HostSpecId,ApplSpecId} ->
		   case  Key of
		       appl_deployment_id->
			   {ok,ApplDeployId};
		       status->
			   {ok,Status};
		       pod_name->
			   {ok,PodName};
		       pod_node->
			   {ok,PodNode};
		       pod_dir->
			   {ok,PodDir};
		       cluster_application_deployment_id->
			   {ok,ClusterApplicationDeploymentId};
		       cluster_deployment_id->
			   {ok,ClusterSpecId};
		       host_spec_id->
			   {ok,HostSpecId};
		       appl_spec_id->
			   {ok,ApplSpecId};
		       Err ->
			   {error,['Key eexists',Err,DeplId,?MODULE,?LINE]}
		   end
	   end,
    Return.

add_info(Key,Info,DeplId)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
				       X#?RECORD.deploy_id==DeplId])),
	     case RecordList of
		 []->
		     mnesia:abort(?TABLE);
		 [S1]->
		     R=case Key of
			    status->
			       NewRecord=S1#?RECORD{status=Info},
			       {ok,S1,NewRecord};
			   Err ->
			       {error,['Key eexists',Err,DeplId,?MODULE,?LINE]}
		       end,
		     case R of 
			 {ok,S,Record}->
			     mnesia:delete_object(S),
			     mnesia:write(Record);
			 {error,Reason}->
			     {error,Reason}
		     end
		 
	     end
	end,
    mnesia:transaction(F).
    
delete_info(Key,_Info,DeplId)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
				       X#?RECORD.deploy_id==DeplId])),
	     case RecordList of
		 []->
		     mnesia:abort(?TABLE);
		 [_S1]->
		     R=case Key of
			   status->
			       {error,[not_applicable,Key]};
			   Err ->
			       {error,['Key eexists',Err,DeplId,?MODULE,?LINE]}
		       end,
		     case R of 
			 {ok,S,Record}->
			     mnesia:delete_object(S),
			     mnesia:write(Record);
			 {error,Reason}->
			     {error,Reason}
		     end
		 
	     end
	end,
    mnesia:transaction(F).
    

get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [X#?RECORD.deploy_id||X<-Z].
    
read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{X#?RECORD.deploy_id,
      X#?RECORD.appl_deployment_id,
      X#?RECORD.status,
      X#?RECORD.pod_name,
      X#?RECORD.pod_node,
      X#?RECORD.pod_dir,
      X#?RECORD.cluster_application_deployment_id,
      X#?RECORD.cluster_deployment_id,
      X#?RECORD.host_spec_id,
      X#?RECORD.appl_spec_id
     }||X<-Z].

read(Object)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.deploy_id==Object])),
    Result=case Z of
	       []->
		  [];
	       [X]->
		   {X#?RECORD.deploy_id,
		    X#?RECORD.appl_deployment_id,
		    X#?RECORD.status,
		    X#?RECORD.pod_name,
		    X#?RECORD.pod_node,
		    X#?RECORD.pod_dir,
		    X#?RECORD.cluster_application_deployment_id,
		    X#?RECORD.cluster_deployment_id,
		    X#?RECORD.host_spec_id,
		    X#?RECORD.appl_spec_id
		   }
	   end,
    Result.



delete(Object) ->
    F = fun() -> 
		mnesia:delete({?TABLE,Object})
		    
	end,
    mnesia:transaction(F).


do(Q) ->
    F = fun() -> qlc:e(Q) end,
    Result=case mnesia:transaction(F) of
	       {atomic, Val} ->
		   Val;
	       {error,Reason}->
		   {error,Reason}
	   end,
    Result.

%%-------------------------------------------------------------------------
