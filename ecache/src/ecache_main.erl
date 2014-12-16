-module(ecache_main).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	start_link/0,
	start/0,
	stop/1,
	cmd/1,	
	cmd/2	
]).

-record(state, {}).

start() ->
	{ok,Pid} = ecache_sup:start_child().

stop(Pid)->
	gen_server:cast(Pid,stop).

start_link() ->
	{ok,Pid} = gen_server:start_link(?MODULE,[],[]).

%% 入口操作
cmd(Args) ->
	{ok,Pid} = start(),
	log4erl:debug("cmd_input ::> ~p",[Args]),	
	Rtn = gen_server:call(Pid,{cmd,Args}),
	stop(Pid),
	Rtn.
cmd(Pid,Args) ->
	log4erl:debug("cmd_input ::> ~p",[Args]),	
	gen_server:call(Pid,{cmd,Args}).

do_cmd(Args)->	
	[Do,K|_] = Args,
	Do1 = case is_binary(Do) of
		true -> 
			erlang:binary_to_list(Do);
		_ ->
			Do
	end,
	%% 2014-12-15 : 这里要统计发消息的数量，需要取得发送人信息
	case string:to_upper(Do1) of
		"PSETEX" ->
			%% TODO 记录发消息的日志，要找到发送人			
			[_,_,_,Packet] = Args,
			{From,_,_} = erlang:binary_to_term(Packet),	
			SF = erlang:tuple_to_list(From),
			[jid,US,SS|_] = SF,
			US1 = case is_binary(US) of true-> binary_to_list(US) ; false -> US end,
			SS1 = case is_binary(SS) of true-> binary_to_list(SS) ; false -> SS end,
			F1 = US1++"@"++SS1,
			log4erl:info("PSETEX\01~p\01~p",[F1,K]);
		_ ->
			log4erl:info("do_cmd_input ::> do=~p ; Key=~p",[Do,K])
	end,
	Result = try
		emsg_redis:q(Args)
	catch 
		_:_ ->
			Err = erlang:get_stacktrace(),
			log4erl:error("exception args=~p ; error=~p",[Args,Err]),
			{error,[]}
	end,
	log4erl:debug("do_cmd_output ::> ~p",[Result]),	
	Result.

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State} | {ok, State, Timeout} | {ok, State, hibernate} | {stop, Reason :: term()} | ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
	{ok,#state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: 	  {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
%% ====================================================================
handle_call({cmd,Args}, From, State) ->
	log4erl:debug("call_input ::> ~p",[Args]),	
	{reply , do_cmd(Args) , State}.


%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(stop, State) ->
	{stop, normal, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({cmd,Args},State)->
	{noreply,State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(Reason, State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


