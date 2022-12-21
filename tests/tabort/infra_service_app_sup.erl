%%%-------------------------------------------------------------------
%% @doc console top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(infra_service_app_sup).

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
     ChildSpecs = [#{id=>db_etcd, 
                     start=>{db_etcd_server,start,[]}},
		  #{id=>nodelog,
		    start=>{nodelog_server,start,[]}},
		  #{id=>resource_discovery,
		    start=>{resource_discovery_server,start,[]}},
		  #{id=>connect,
		    start=>{connect_server,start,[]}},
		  #{id=>appl,
		    start=>{appl_server,start,[]}},
		  #{id=>pod,
		    start=>{pod_server,start,[]}},
		  #{id=>oam,
		    start=>{oam_server,start,[]}},
		  #{id=>infra_service,
                     start=>{infra_service_server,start,[]}}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
