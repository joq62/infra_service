-module(db_appl_instance).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_appl_instance.hrl").

create_table()->
    mnesia:create_table(?TABLE, [{attributes, record_info(fields, ?RECORD)},
				 {type,bag}
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

%% Special functions


%%-------------------------------------------------------------------------------------

create(ClusterInstance,ApplSpec,PodNode,HostSpec,Status)->
    Record=#?RECORD{
		    cluster_instance=ClusterInstance,
		    appl_spec=ApplSpec,
		    pod_node=PodNode,
		    host_spec=HostSpec,
		    status=Status

		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

member(ClusterInstance)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_instance==ClusterInstance])),
    Member=case Z of
	       []->
		   false;
	       _->
		   true
	   end,
    Member.



read(Key,ClusterInstance,PodNode)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_instance==ClusterInstance,
		     X#?RECORD.pod_node==PodNode])),
    Return=case Z of
	       []->
		   [];
	       Z->
		   case  Key of
		       cluster_instance->
			   [X|_]=Z,
			   {ok,X#?RECORD.cluster_instance };
		       appl_spec->
			   R=[X#?RECORD.appl_spec||X<-Z],
			   {ok,R};
		       pod_node->
			   PodNode;
		       host_spec->
			   [X|_]=Z,
			   {ok,X#?RECORD.host_spec};
		       status->
			   R=[X#?RECORD.status||X<-Z],
			   {ok,R};
		       Err ->
			   {error,['Key eexists',Err,ClusterInstance,PodNode,?MODULE,?LINE]}
		   end
	   end,
    Return.



get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [ClusterInstance||{?RECORD,ClusterInstance,_ApplSpec,_PodNode,_Status}<-Z].
    
read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    Result=[{X#?RECORD.cluster_instance,X#?RECORD.appl_spec,X#?RECORD.pod_node,X#?RECORD.host_spec,X#?RECORD.status}||X<-Z],
 
    Result.

get_pod_appl_specs(ClusterInstance)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_instance==ClusterInstance])),
    [{X#?RECORD.appl_spec,X#?RECORD.pod_node}||X<-Z].
    
read(ClusterInstance)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_instance==ClusterInstance])),
    [{X#?RECORD.cluster_instance,X#?RECORD.appl_spec,X#?RECORD.pod_node,X#?RECORD.host_spec,X#?RECORD.status}||X<-Z].
    


read(ClusterInstance,PodNode)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_instance==ClusterInstance,
		     X#?RECORD.pod_node==PodNode])),
    [{X#?RECORD.cluster_instance,X#?RECORD.appl_spec,X#?RECORD.pod_node,X#?RECORD.host_spec,X#?RECORD.status}||X<-Z].
   
 

delete(ClusterInstance,ApplSpec,PodNode) ->
    F = fun() -> 
		Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
				 X#?RECORD.cluster_instance==ClusterInstance,
				 X#?RECORD.appl_spec==ApplSpec,
				 X#?RECORD.pod_node==PodNode])),
		Result=case Z of
			   []->
			       mnesia:abort({error,[eexists_record,ClusterInstance,PodNode,?MODULE,?LINE]});
			   [X]->
			       mnesia:delete_object(?TABLE, X, write)    
		       end
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
