-module(db_appl_deployment).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_appl_deployment.hrl").

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

create(SpecId,ApplSpec,Vsn,ClusterSpec,NumInstances,Affinity)->
    Record=#?RECORD{
		    spec_id=SpecId,
		    appl_spec=ApplSpec,
		    vsn=Vsn,
		    cluster_spec=ClusterSpec,
		    num_instances=NumInstances,
		    affinity=Affinity
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
	       {_SpecId,ApplSpec,Vsn,ClusterSpec,NumInstances,Affinity} ->
		   case  Key of
		       appl_spec->
			   {ok,ApplSpec};
		       vsn->
			   {ok,Vsn};
		       cluster_spec->
			   {ok,ClusterSpec};
		       num_instances->
			   {ok,NumInstances};
		       affinity->
			   {ok,Affinity};
		       Err ->
			   {error,['Key eexists',Err,SpecId,?MODULE,?LINE]}
		   end
	   end,
    Return.


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [SpecId||{?RECORD,SpecId,_ApplSpec,_Vsn,_ClusterSpec,_NumInstances,_Affinity}<-Z].
    
read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{SpecId,ApplSpec,Vsn,ClusterSpec,NumInstances,Affinity}||{?RECORD,SpecId,ApplSpec,Vsn,ClusterSpec,NumInstances,Affinity}<-Z].

read(Object)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.spec_id==Object])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{SpecId,ApplSpec,Vsn,ClusterSpec,NumInstances,Affinity}||{?RECORD,SpecId,ApplSpec,Vsn,ClusterSpec,NumInstances,Affinity}<-Z],
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
    GitDir=filename:join(TempDirName,?ApplDeploymentDir),
    GitPath=?GitPathApplDeployments,
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
    from_file(?ApplDeploymentDir).

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
	       {ok,[{appl_deployment,SpecId,Info}]}->
		   {appl_spec,ApplSpec}=lists:keyfind(appl_spec,1,Info),
		   {vsn,Vsn}=lists:keyfind(vsn,1,Info),
		   {cluster_spec,ClusterSpec}=lists:keyfind(cluster_spec,1,Info),
		   {num_instances,NumInstances}=lists:keyfind(num_instances,1,Info),
		   {affinity,Affinity}=lists:keyfind(affinity,1,Info),
		   case create(SpecId,ApplSpec,Vsn,ClusterSpec,NumInstances,Affinity) of
		       {atomic,ok}->
			   [{ok,FileName}|Acc];
		       {error,Reason}->
			   [{error,[Reason,FileName,Dir,?MODULE,?LINE]}|Acc]
		   end;
	       {ok,NotAnApplSpecFile} -> 
		   [{error,[not_appl_spec_file,NotAnApplSpecFile,FileName,Dir,?MODULE,?LINE]}|Acc]
	   end,
    from_file(T,Dir,NewAcc).
