-module(db_cluster_instance).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_cluster_instance.hrl").

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

nodes(Type,ClusterSpec)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		  X#?RECORD.cluster_spec==ClusterSpec,
		     X#?RECORD.type==Type])),
    [X#?RECORD.pod_node||X<-Z].

pod_based_host_spec(HostSpec,Type,ClusterSpec)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_spec==ClusterSpec,
		     X#?RECORD.host_spec==HostSpec,
		     X#?RECORD.type==Type])),
    [X#?RECORD.pod_node||X<-Z].
    

%%-------------------------------------------------------------------------------------

create(ClusterSpec,Type,PodName,PodNode,PodDir,HostSpec,Status)->
    Record=#?RECORD{
		    cluster_spec=ClusterSpec,
		    type=Type,
		    pod_name=PodName,
		    pod_node=PodNode,
		    pod_dir=PodDir,
		    host_spec=HostSpec,
		    status=Status

		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

member(ClusterSpec)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_spec==ClusterSpec])),
    Member=case Z of
	       []->
		   false;
	       _->
		   true
	   end,
    Member.



read(Key,ClusterSpec,PodNode)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_spec==ClusterSpec,
		     X#?RECORD.pod_node==PodNode])),
    Return=case Z of
	       []->
		   [];
	       [X]->
		   case  Key of
		       cluster_spec->
			   {ok,X#?RECORD.cluster_spec};
		       type->
			  {ok,X#?RECORD.type};
		       pod_name->
			   {ok,X#?RECORD.pod_name};
		       pod_node->
			    {ok,X#?RECORD.pod_node};
		       pod_dir->
			    {ok,X#?RECORD.pod_dir};
		       host_spec->
			    {ok,X#?RECORD.host_spec};
		       status->
			   {ok,X#?RECORD.status};
		       Err ->
			   {error,['Key eexists',Err,ClusterSpec,?MODULE,?LINE]}
		   end
	   end,
    Return.


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [ClusterSpec||{?RECORD,ClusterSpec,_ConnectNode,_PodName,_PodNode,_PodDir,_HostSpec,_Status}<-Z].
    
read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    Result=[{X#?RECORD.cluster_spec,X#?RECORD.type,X#?RECORD.pod_name,X#?RECORD.pod_node,X#?RECORD.pod_dir,X#?RECORD.host_spec,X#?RECORD.status}||X<-Z],
 
    Result.

read(ClusterSpec)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_spec==ClusterSpec])),
    [{X#?RECORD.cluster_spec,X#?RECORD.type,X#?RECORD.pod_name,X#?RECORD.pod_node,X#?RECORD.pod_dir,X#?RECORD.host_spec,X#?RECORD.status}||X<-Z].
    


read(ClusterSpec,PodNode)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.cluster_spec==ClusterSpec,
		     X#?RECORD.pod_node==PodNode])),
    
   
    Result=case Z of
	       []->
		   [];
	       [X]->
		   {X#?RECORD.cluster_spec,X#?RECORD.type,X#?RECORD.pod_name,X#?RECORD.pod_node,X#?RECORD.pod_dir,X#?RECORD.host_spec,X#?RECORD.status}
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
