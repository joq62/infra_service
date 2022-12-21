-module(db_appl_state).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_appl_state.hrl").

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

create(DeplId,ApplName,PodsInfo,DeployInfo)->
    %io:format("DeplId,ApplName,PodsInfo,DeployInfo ~p~n",[{DeplId,ApplName,PodsInfo,DeployInfo,?MODULE,?FUNCTION_NAME}]),
    Record=#?RECORD{
		    deployment_id=DeplId,
		    appl_name=ApplName,
		    pods=[PodsInfo],
		    deployment_info=[DeployInfo]
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

member(DeplId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.deployment_id==DeplId])),
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
	       {_SpecId,ApplName,PodsInfo,DeployInfo} ->
		   case  Key of
		       appl_name->
			   {ok,ApplName};
		       pods->
			   {ok,PodsInfo};
		       deployment_info->
			   {ok,DeployInfo};
		       Err ->
			   {error,['Key eexists',Err,DeplId,?MODULE,?LINE]}
		   end
	   end,
    Return.

add_info(Key,Info,DeplId)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
				       X#?RECORD.deployment_id==DeplId])),
	     case RecordList of
		 []->
		     mnesia:abort(?TABLE);
		 [S1]->
		     R=case Key of
			   appl_name->
			       NewRecord=S1#?RECORD{appl_name=Info},
			       {ok,S1,NewRecord};
			   pods->
			       NewPods=[Info|lists:delete(Info,S1#?RECORD.pods)],
			       NewRecord=S1#?RECORD{pods=NewPods},
			       {ok,S1,NewRecord};
			   deployment_info->
			       NewDeplInfo=[Info|lists:delete(Info,S1#?RECORD.deployment_info)],
			       NewRecord=S1#?RECORD{deployment_info=NewDeplInfo},
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
    
delete_info(Key,Info,DeplId)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
				       X#?RECORD.deployment_id==DeplId])),
	     case RecordList of
		 []->
		     mnesia:abort(?TABLE);
		 [S1]->
		     R=case Key of
			   appl_name->
			       {error,[not_applicable,Key]};
			   pods->
			       NewPods=lists:delete(Info,S1#?RECORD.pods),
			       NewRecord=S1#?RECORD{pods=NewPods},
			       {ok,S1,NewRecord};
			   deployment_info->
			       NewDeplInfo=lists:delete(Info,S1#?RECORD.deployment_info),
			       NewRecord=S1#?RECORD{deployment_info=NewDeplInfo},
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
    

get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [Id||{?RECORD,Id,_ApplName,_PodsInfo,_DeployInfo}<-Z].
    
read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{X#?RECORD.deployment_id,
      X#?RECORD.appl_name,
      X#?RECORD.pods,
      X#?RECORD.deployment_info}||X<-Z].

read(Object)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.deployment_id==Object])),
    Result=case Z of
	       []->
		  [];
	       [X]->
		   {X#?RECORD.deployment_id,
		    X#?RECORD.appl_name,
		    X#?RECORD.pods,
		    X#?RECORD.deployment_info}
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
