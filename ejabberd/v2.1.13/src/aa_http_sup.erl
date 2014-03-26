-module(aa_http_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	AA ={
		aa_http,{aa_http, start_link, []},
		permanent,
		brutal_kill,
		worker,
		[aa_http]
	},
    {ok, {{one_for_one, 5, 10}, [AA]}}.
%%%===================================================================
%%% Internal functions
%%%===================================================================
