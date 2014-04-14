-module(ecache_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,start_child/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child()->
	supervisor:start_child(?MODULE,[]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	AChild = {ecache_main, {ecache_main,start_link,[]}, temporary, 2000, worker, [ecache_main] },
	{ok,{{simple_one_for_one,0,1}, [AChild]}}.

