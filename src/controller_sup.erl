%%%-------------------------------------------------------------------
%% @doc ops_node top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(controller_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
     ChildSpecs = [#{id=>common,
		    start=>{common,start,[]}},
		  #{id=>resource_discovery,
		    start=>{resource_discovery_server,start,[]}},
		   #{id=>db_etcd,
		    start=>{db_etcd_server,start,[]}},
		   #{id=>ops_connect_operator,
		     start=>{ops_connect_operator_server,start,[]}},
		   #{id=>ops_pod_operator,
		     start=>{ops_pod_operator_server,start,[]}},
		   #{id=>ops_appl_operator,
		     start=>{ops_appl_operator_server,start,[]}},
		   #{id=>oam,
		     start=>{oam,start,[]}},
		   #{id=>ops_node,
		     start=>{ops_node_server,start,[]}}		   
		 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
