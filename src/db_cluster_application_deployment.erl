-module(db_cluster_application_deployment).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_cluster_application_deployment.hrl").

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

create(SpecId,ClusterSpec,ApplDeploySpecs)->
    Record=#?RECORD{
		    spec_id=SpecId,
		    cluster_spec=ClusterSpec,
		    appl_deployment_specs=ApplDeploySpecs
		   },
    F = fun() -> mnesia:write(Record) end,
    mnesia:transaction(F).

member(SpecId)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.spec_id==SpecId])),
    Member=case Z of
	       []->
		   false;
	       _->
		   true
	   end,
    Member.

read(Key,SpecId)->
    Return=case read(SpecId) of
	       []->
		   {error,[eexist,SpecId,?MODULE,?LINE]};
	       {_SpecId,ClusterSpec,ApplDeploySpecs} ->
		   case  Key of
		      cluster_spec->
			   {ok,ClusterSpec};
		       appl_deployment_specs->
			   {ok,ApplDeploySpecs};
		       Err ->
			   {error,['Key eexists',Err,SpecId,?MODULE,?LINE]}
		   end
	   end,
    Return.

add_info(Key,Info,SpecId)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
				       X#?RECORD.spec_id==SpecId])),
	     case RecordList of
		 []->
		     mnesia:abort(?TABLE);
		 [S1]->
		     R=case Key of
			   cluster_spec->
			       NewRecord=S1#?RECORD{cluster_spec=Info},
			       {ok,S1,NewRecord};
			   appl_deployment_specs->
			       NewApplDeploySpecs=[Info|lists:delete(Info,S1#?RECORD.appl_deployment_specs)],
			       NewRecord=S1#?RECORD{appl_deployment_specs=NewApplDeploySpecs},
			       {ok,S1,NewRecord};
			   Err ->
			       {error,['Key eexists',Err,SpecId,?MODULE,?LINE]}
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
    
delete_info(Key,Info,SpecId)->
    F = fun() -> 
		RecordList=do(qlc:q([X || X <- mnesia:table(?TABLE),
				       X#?RECORD.spec_id==SpecId])),
	     case RecordList of
		 []->
		     mnesia:abort(?TABLE);
		 [S1]->
		     R=case Key of
			   cluster_spec->
			       {error,[not_applicable,Key]};
			   appl_deployment_specs->
			       NewApplDeploySpecs=lists:delete(Info,S1#?RECORD.appl_deployment_specs),
			       NewRecord=S1#?RECORD{appl_deployment_specs=NewApplDeploySpecs},
			       {ok,S1,NewRecord};
			   Err ->
			       {error,['Key eexists',Err,SpecId,?MODULE,?LINE]}
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
    [SpecId||{?RECORD,SpecId,_ClusterSpec,_ApplDeploySpecs}<-Z].
    
read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{SpecId,ClusterSpec,ApplDeploySpecs}||{?RECORD,SpecId,ClusterSpec,ApplDeploySpecs}<-Z].

read(Object)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.spec_id==Object])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{SpecId,ClusterSpec,ApplDeploySpecs}||{?RECORD,SpecId,ClusterSpec,ApplDeploySpecs}<-Z],
		   Info
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
git_clone_load()->
    ok=create_table(),
    Result=case git_clone() of
	       {error,Reason}->
		   {error,Reason};
	       {ok,TempDirName,SpecDir}->
		   case from_file(SpecDir) of
		       {error,Reason}->
			   os:cmd("rm -rf "++TempDirName),	
			   {error,Reason};
		       LoadResult->
			   os:cmd("rm -rf "++TempDirName),	
			   LoadResult
		   end
	   end,
    Result.

git_clone()->
    TempDirName=erlang:integer_to_list(os:system_time(microsecond),36)++".dir",
    ok=file:make_dir(TempDirName),
    GitDir=filename:join(TempDirName,?ClusterApplDeploymentDir),
    GitPath=?GitPathClusterApplDeployments,
    os:cmd("rm -rf "++GitDir),    
    ok=file:make_dir(GitDir),
    GitResult=appl:git_clone_to_dir(node(),GitPath,GitDir),
    Result=case filelib:is_dir(GitDir) of
	       false->
		   {error,[failed_to_clone,GitPath,GitResult]};
	       true->
		   {ok,TempDirName,GitDir}
	   end,
    Result.	

from_file()->
    from_file(?ClusterApplDeploymentDir).

from_file(Dir)->
    {ok,FileNames}=file:list_dir(Dir),
    from_file(FileNames,Dir,[]).

from_file([],_,Acc)->
    Acc;		     
from_file([FileName|T],Dir,Acc)->
    FullFileName=filename:join(Dir,FileName),
    NewAcc=case file:consult(FullFileName) of
	       {error,Reason}->
		   [{error,[Reason,FileName,Dir,?MODULE,?LINE]}|Acc];
	       {ok,[{cluster_application_deployment,SpecId,Info}]}->
		   {cluster_spec,ClusterSpec}=lists:keyfind(cluster_spec,1,Info),
		   {appl_deployment_specs,ApplDeploySpecs}=lists:keyfind(appl_deployment_specs,1,Info),
		   case create(SpecId,ClusterSpec,ApplDeploySpecs) of
		       {atomic,ok}->
			   [{ok,FileName}|Acc];
		       {error,Reason}->
			   [{error,[Reason,FileName,Dir,?MODULE,?LINE]}|Acc]
		   end;
	       {ok,NotAnApplSpecFile} -> 
		   [{error,[not_appl_spec_file,NotAnApplSpecFile,FileName,Dir,?MODULE,?LINE]}|Acc]
	   end,
    from_file(T,Dir,NewAcc).
	
  
