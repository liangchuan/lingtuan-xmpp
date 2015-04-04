%% @author Administrator
%% @doc @todo Add description to aa_mongo_senter.


-module(aa_mongo_senter).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").

-define(MONGO_POOL,mongo_pool).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,sysn_write/1,read_opt_uidlist/0]).


start_link()->
	gen_server:start_link(?MODULE,[],[]).

sysn_write(Data)->
	if
		Data =:= []->
			ok;
		true->
			{ok,Pid} = start(),
			gen_server:cast(Pid,{sysn_write,Data})
	end.

%%%%%%%%%%%%%%%%%%%%%%%%test  mongodb  test
%%TODO
%% test_one(N,Type)->
%% 	Table = list_to_binary("test_mongodb"++erlang:atom_to_list(Type)++""),
%% 	List = lists:seq(1, N),
%% 	Data = 
%% 	lists:map(fun(NN)->
%% 				{id,NN,teststr,<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>}	  
%% 			  end, List),
%% 	{ok,ConnectPid} = mongo_pool:checkout(?MONGO_POOL),
%% 	if
%% 		Type =:= once->
%% 			Time = now(),
%% 			lists:foreach(fun(D)->
%% 							mongo:insert(ConnectPid, Table, D)	  
%% 						  end, Data),
%% 			Diff = timer:now_diff(now(), Time),
%% 			?WARNING_MSG("diff time:~p",[Diff]);
%% 		Type =:= batch->
%% 			Time = now(),
%% 			mongo:insert(ConnectPid, Table, Data),
%% 			Diff = timer:now_diff(now(), Time),
%% 			?WARNING_MSG("diff time:~p",[Diff]);
%% 		Type =:= moreonce->
%% 			Time = now(),
%% 			lists:foreach(fun(D)->
%% 							{ok,ConnectPidddd} = mongo_pool:checkout(?MONGO_POOL),
%% 							mongo:insert(ConnectPidddd, Table, D)	  
%% 						  end, Data),
%% 			Diff = timer:now_diff(now(), Time),
%% 			?WARNING_MSG("diff time:~p",[Diff]);
%% 		Type =:= moreonce2->
%% 			Diff =
%% 			lists:foldl(fun(D,Acc)->
%% 							{ok,ConnectPidddd} = mongo_pool:checkout(?MONGO_POOL),
%% 							Time = now(),
%% 							mongo:insert(ConnectPidddd, Table, D),
%% 							timer:now_diff(now(), Time)+Acc	  
%% 						  end,0, Data),
%% 			?WARNING_MSG("diff time:~p",[Diff]);
%% 		true->
%% 			skip
%% 	end.




%%%获取需要的运营账号
%%%此逻辑在aa_mongodb进程中执行，写在这里只是为了管理方便
read_opt_uidlist()->		
	Table = <<"onlineuser">>,
	Selector = {lid,1},
	Projector = {loginid,true},
	{ok,ConnectPid} = mongo_pool:checkout(?MONGO_POOL),
	try
		 case mongo:find_one(ConnectPid, Table, Selector, Projector) of
			{MongoData}->
				case bson:lookup(loginid, MongoData) of
					{}->
						[];
					{Userlist}->
						Userlist
				end;
			 {}->
				[]
		 end
	catch
		_:_->
			?WARNING_MSG("mongo db opt uidlist data error!!!",[]),
			[]
	end.
	

	
	
start() ->
	aa_mongo_senter_sup:start_child().

%% stop(Pid)->
%% 	gen_server:cast(Pid,stop).
%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([]) ->
    {ok, #state{}}.


%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


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
handle_cast({sysn_write,Data},State)->
	{ok,ConnectPid} = mongo_pool:checkout(?MONGO_POOL),
	Conf = ejabberd_config:get_local_option({mongodb,?MYNAME}),
	{ok,Col} =  dict:find(collect,dict:from_list(Conf)),
	mongo:insert(ConnectPid, Col, Data),
	{noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


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
handle_info(_Info, State) ->
    {noreply, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
%% ====================================================================
terminate(_Reason, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


