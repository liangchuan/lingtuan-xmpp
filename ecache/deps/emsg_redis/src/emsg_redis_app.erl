%%%-------------------------------------------------------------------
%%% @author Jeremy Ong <jeremy@playmesh.com>
%%% @copyright (C) 2012, PlayMesh, Inc.
%%%-------------------------------------------------------------------

-module(emsg_redis_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    emsg_redis_sup:start_link().

stop(_State) ->
    ok.
