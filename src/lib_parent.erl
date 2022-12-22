%%% -------------------------------------------------------------------
%%% Author  : uabjle
%%% Description : resource discovery accroding to OPT in Action 
%%% This service discovery is adapted to 
%%% Type = application 
%%% Instance ={ip_addr,{IP_addr,Port}}|{erlang_node,{ErlNode}}
%%% 
%%% Created : 10 dec 2012
%%% -------------------------------------------------------------------
-module(lib_parent).
 

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------

%% --------------------------------------------------------------------

%% External exports
-export([
	 load_desired_state/1

	]).


%% ====================================================================
%% External functions
%% ====================================================================
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

load_desired_state(ClusterSpec)->
    {ok,Pods}=db_cluster_spec:read(pods,ClusterSpec),
    LoadResult=[{error,Reason}|| {error,Reason}<-load_desired_state(Pods,ClusterSpec,[])],
    case LoadResult of
	[]->
	    ok;
	ErrorList ->
	    {error,ErrorList}
    end.
    
load_desired_state([],_ClusterSpec,Acc)->
    Acc;
load_desired_state([{_NumPods,HostSpec}|T],ClusterSpec,Acc) ->
    false=lists:member({ok,HostSpec},Acc),
    {ok,RootDir}=db_cluster_spec:read(root_dir,ClusterSpec),
    {ok,HostName}=db_host_spec:read(hostname,HostSpec),
    NodeName=ClusterSpec++"_parent",
    ParentNode=list_to_atom(NodeName++"@"++HostName),
    RootPaArgs=" -pa "++RootDir++" ",
    PathCommonFuns=filename:join([RootDir,"*","ebin"]),
    CommonFunsPaArgs=" -pa "++PathCommonFuns,
    EnvArgs=" ",
    Result=case db_parent_desired_state:create(ParentNode,NodeName,ClusterSpec,HostSpec,
					       RootPaArgs,CommonFunsPaArgs,EnvArgs) of
	       {atomic,ok}->
		   ok;
	       Reason->
		   {error,Reason}
	   end,
    load_desired_state(T,ClusterSpec,[Result|Acc]).
