-module(ecache_run).
-export([start/0]).
start()-> application:start(ecache).

