%%%-------------------------------------------------------------------
%%% @author Jeremy Ong <jeremy@playmesh.com>
%%% @copyright (C) 2012, PlayMesh, Inc.
%%%-------------------------------------------------------------------

-module(emsg_redis_sup).

-behaviour(supervisor).

%% Include
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API functions

start_link() ->
    {ok, Pools} = application:get_env(emsg_redis, pools),
    start_link(Pools).

start_link(Pools) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Pools]).

%% Supervisor callbacks

init([Pools]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 5000,
    Type = worker,

    emsg_redis_chash:store_ring(),

    PoolSpecs = lists:map(fun({PoolName, PoolConfig}) ->
        {PoolName, {emsg_redis_pool, start_link, [{PoolName,PoolConfig}]}, Restart, Shutdown, Type, []}
    end, Pools),
    {ok, {SupFlags, PoolSpecs}}.
