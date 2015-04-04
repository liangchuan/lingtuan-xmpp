%% @author Administrator
%% @doc @todo Add description to aa_mongo_sup.


-module(aa_mongo_sup).
-behaviour(supervisor).
-export([init/1]).

-include("ejabberd.hrl").
-define(MONGO_POOL,mongo_pool).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0]).

start_link() ->
	Conf = ejabberd_config:get_local_option({mongodb,?MYNAME}),
	?INFO_MSG("mongodb:~p",[{Conf,?MYNAME}]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Conf]).

%% ====================================================================
init([Conf]) ->
	Mc_super_sup = {mc_super_sup,{mc_super_sup, start_link,[]},
						   temporary,brutal_kill, supervisor, [mc_super_sup] },
	Mongo_pool = {mongo_pool,{mongo_pool,start_link,[{?MONGO_POOL,Conf}]},
	     				   permanent,infinity,worker,[mongo_pool]},
    Aa_mongodb = {aa_mongodb,{aa_mongodb,start_link,[]},
	     				   permanent,infinity,worker,[aa_mongodb]},
	Aa_mongo_senter_sup = {aa_mongo_senter_sup,{aa_mongo_senter_sup, start_link,[]},
						   temporary,brutal_kill, supervisor, [aa_mongo_senter_sup] },
    {ok,{{one_for_one,5,10}, [Mc_super_sup,Mongo_pool,Aa_mongodb,Aa_mongo_senter_sup]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================


