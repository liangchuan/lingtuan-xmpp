-module(ecache_app).

-behaviour(application).

-define(log_file,"ecache_app").
-define(log_level,info).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
		[T1,T2,_] = tuple_to_list(os:timestamp()),
		Now = integer_to_list(T1)++integer_to_list(T2),
		application:start(log4erl),
		%% sharded_eredis:start(),
		emsg_redis:start(),
		%% log4erl:add_file_appender(ecache_server, {".", ?log_file, {size, 1000000000}, 4, "log", ?log_level,"%j %T [%L] %l%n"}),
		log4erl:conf("log4e.conf"),
		log4erl:info("ecache start at :::> ~p",[Now]),
		{ok,Cookie} = application:get_env(ecache,erlang_cookie),
		log4erl:debug("ecache cookie :::> ~p",[Cookie]),
		erlang:set_cookie(node(),Cookie),
		ecache_sup:start_link().

stop(_State) -> ok.
