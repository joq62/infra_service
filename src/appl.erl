%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : dbase using dets 
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(appl).  
   
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Records for test
%%

%% --------------------------------------------------------------------
%-compile(export_all).
-export([
	 git_clone/3,
	 git_clone_to_dir/3,
	 load/3,
	 unload/3,
	 start/2,
	 start/3,
	 start/4,
	 stop/2
	]).
	 

%% ====================================================================
%% External functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
git_clone_to_dir(Node,GitPath,DirToClone)->
    case rpc:call(Node,file,get_cwd,[],5000) of  
	{badrpc,Reason}->
	    {error,[badrpc,Reason]};
	  {ok,Root}->
	    CloneDir=filename:join(Root,DirToClone),
	 %   io:format("CloneDir ~p~n",[CloneDir]),
	    TempDirName=erlang:integer_to_list(os:system_time(microsecond),36)++".dir",
	    TempDir=filename:join(Root,TempDirName),
	  %  io:format("TempDir ~p~n",[TempDir]),
	    case rpc:call(Node,os,cmd,["rm -rf "++TempDir],5000) of
		{badrpc,Reason}->
		    {error,[badrpc,Reason]};
		[]->
		    case rpc:call(Node,file,make_dir,[TempDir],5000) of
			{badrpc,Reason}->
			    {error,[badrpc,Reason]};
			ok->
			    case rpc:call(Node,os,cmd,["git clone "++GitPath++" "++TempDir],5000) of
				{badrpc,Reason}->
				    {error,[badrpc,Reason]};
				CloneResult->
				    case rpc:call(Node,os,cmd,["mv  "++TempDir++"/*"++" "++CloneDir],5000) of
					{badrpc,Reason}->
					    {error,[badrpc,Reason,CloneResult]};
					[]->
					    case rpc:call(Node,os,cmd,["rm -r  "++TempDir],5000) of
						{badrpc,Reason}->
						    {error,[badrpc,Reason,CloneResult]};
						[]->
						    {ok,CloneDir}
					    end;
					Reason ->
					    {error,[Reason,{dir_to_clone,DirToClone},CloneResult]}
				    end
			    end
		    end
	    end
    end.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
git_clone(Node,GitPath,GitDir)->
    case rpc:call(Node,filelib,is_dir,[GitDir],2000) of
	{badrpc,Reason}->
	    {error,[badrpc,Reason]};
	true->
	    {error,[already_exists,GitDir]};
	false->
	    case rpc:call(Node,os,cmd,["git clone "++GitPath], 10*1000) of
		{badrpc,Reason}->
		    {error,[badrpc,Reason]};
		GitResult->
		    case rpc:call(Node,filelib,is_dir,[GitDir],5000) of
			{badrpc,Reason}->
			    {error,[badrpc,Reason]};
			false->
			    {error,[failed_to_clone,GitPath,GitResult]};
			true->
			    {ok,GitDir}
		    end
	    end
    end.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
load(Node,App,Paths)->
    AddedPathsResult=[{Path,rpc:call(Node,code,add_patha,[Path],5000)}||Path<-Paths,
									true/=rpc:call(Node,code,add_patha,[Path],5000)],
    case AddedPathsResult of
	[]-> % ok
	    case rpc:call(Node,application,load,[App],5000) of
		{badrpc,Reason}->
		    {error,[badrpc,Reason,App]};
		{error, Reason}->
		    {error, [Reason,App]};
		ok->
		    ok
	    end;
	ErrorList->
	    {error,[ErrorList]}
    end.


%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
stop(Node,App)->
    rpc:call(Node,application,stop,[App],5000).
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
unload(Node,App,Dir)->
    rpc:call(Node,application,unload,[App],2*5000), 
    rpc:call(Node,os,cmd,["rm -rf "++Dir],2*5000),
    ok.
%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
-define(TimeOut,2*5000).
start(Node,App)->
    start(Node,App,?TimeOut).

start(Node,App,TimeOut)->
    case rpc:call(Node,application,start,[App],TimeOut) of
	{badrpc,Reason}->
	    {error,[badrpc,Reason,App]};
	{error, Reason}->
	    {error, Reason,App};
	ok->
	    ok
    end.

%% --------------------------------------------------------------------
%% Function:start/0 
%% Description: Initiate the eunit tests, set upp needed processes etc
%% Returns: non
%% --------------------------------------------------------------------
start(Node,StartModule,StartFun,StartArgs)->
    case rpc:call(Node,StartModule,StartFun,StartArgs,5000) of
	{badrpc,Reason}->
	    {error,[badrpc,Reason,StartModule]};
	{error, Reason}->
	    {error, Reason,StartModule};
	ok->
	    ok
    end.
	       

