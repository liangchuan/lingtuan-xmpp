-module(aa_group_chat_sup).
-behaviour(supervisor).
-export([init/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,start_child/0]).

-define(SERVER,?MODULE).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
-spec init(Args :: term()) -> Result when
	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
	RestartStrategy :: one_for_all | one_for_one | rest_for_one | simple_one_for_one,
	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
	RestartPolicy :: permanent | transient | temporary,
	Modules :: [module()] | dynamic.
init([]) ->
	AChild = {aa_group_chat, {aa_group_chat,start_link,[]}, temporary, 2000, worker, [aa_group_chat] },
	?DEBUG("###### aa_group_chat_sup ::::> ~p",[started]),
    	{ok,{{simple_one_for_one,0,1}, [AChild]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).
start_child()->
	supervisor:start_child(?SERVER,[]).
