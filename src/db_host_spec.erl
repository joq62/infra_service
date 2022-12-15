-module(db_host_spec).
-import(lists, [foreach/2]).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").
-include("db_host_spec.hrl").

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

create(SpecId,HostName,LocalIp,SshPort,Uid,Passwd,ApplConfig)->
    Record=#?RECORD{
		    spec_id=SpecId,
		    hostname=HostName,
		    local_ip=LocalIp,
		    ssh_port=SshPort,
		    uid=Uid,
		    passwd=Passwd,
		    application_config=ApplConfig
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
	       {_SpecId,HostName,LocalIp,SshPort,Uid,Passwd,ApplConfig} ->
		   case  Key of
		       hostname->
			   {ok,HostName};
		       local_ip->
			   {ok,LocalIp};
		       ssh_port->
			   {ok,SshPort};
		       uid->
			   {ok,Uid};
		       passwd->
			   {ok,Passwd};
		       application_config->
			   {ok,ApplConfig};
		       Err ->
			   {error,['Key eexists',Err,SpecId,?MODULE,?LINE]}
		   end
	   end,
    Return.


get_all_id()->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [SpecId||{?RECORD,SpecId,_HostName,_LocalIp,_SshPort,_Uid,_Passwd,_ApplConfig}<-Z].
    
read_all() ->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE)])),
    [{SpecId,HostName,LocalIp,SshPort,Uid,Passwd,ApplConfig}||{?RECORD,SpecId,HostName,LocalIp,SshPort,Uid,Passwd,ApplConfig}<-Z].

read(Object)->
    Z=do(qlc:q([X || X <- mnesia:table(?TABLE),		
		     X#?RECORD.spec_id==Object])),
    Result=case Z of
	       []->
		  [];
	       _->
		   [Info]=[{SpecId,HostName,LocalIp,SshPort,Uid,Passwd,ApplConfig}||{?RECORD,SpecId,HostName,LocalIp,SshPort,Uid,Passwd,ApplConfig}<-Z],
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
    GitDir=filename:join(TempDirName,?HostSpecDir),
    GitPath=?GitPathHostSpecs,
    os:cmd("rm -rf "++GitDir),    
    ok=file:make_dir(GitDir),
    GitResult=appl:git_clone_to_dir(node(),GitPath,GitDir),
    Result=case filelib:is_dir(GitDir) of
	       false->
		   {error,[failed_to_clone,GitPath,GitResult]};
	       true->
		   {ok,TempDirName,GitDir}
	   end,
    Result.	from_file()->
    from_file(?HostSpecDir).

from_file(ApplSpecDir)->
    {ok,FileNames}=file:list_dir(ApplSpecDir),
    from_file(FileNames,ApplSpecDir,[]).

from_file([],_,Acc)->
    Acc;		     
from_file([FileName|T],Dir,Acc)->
    FullFileName=filename:join(Dir,FileName),
    NewAcc=case file:consult(FullFileName) of
	       {error,Reason}->
		   [{error,[Reason,FileName,Dir,?MODULE,?LINE]}|Acc];
	       {ok,[{host_spec,SpecId,Info}]}->
		   {hostname,HostName}=lists:keyfind(hostname,1,Info),
		   {local_ip,LocalIp}=lists:keyfind(local_ip,1,Info),
		   {ssh_port,SshPort}=lists:keyfind(ssh_port,1,Info),
		   {uid,Uid}=lists:keyfind(uid,1,Info),
		   {passwd,Passwd}=lists:keyfind(passwd,1,Info),
		   {application_config,ApplConfig}=lists:keyfind(application_config,1,Info),
		 
		 
		   case create(SpecId,HostName,LocalIp,SshPort,Uid,Passwd,ApplConfig) of
		       {atomic,ok}->
			   [{ok,FileName}|Acc];
		       {error,Reason}->
			   [{error,[Reason,FileName,Dir,?MODULE,?LINE]}|Acc]
		   end;
	       {ok,NotAnApplSpecFile} -> 
		   [{error,[not_appl_spec_file,NotAnApplSpecFile,FileName,Dir,?MODULE,?LINE]}|Acc]
	   end,
    from_file(T,Dir,NewAcc).
			   
