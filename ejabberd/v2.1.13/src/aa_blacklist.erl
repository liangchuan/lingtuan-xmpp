-module(aa_blacklist).
-behaviour(gen_server).

-include_lib("stdlib/include/qlc.hrl").
-include("ejabberd.hrl").
-include("jlib.hrl").

%% API
-export([start_link/0,add/2,remove/2,get_list/1,get_with/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(blacklist, {key,ct}).
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add(From,To) ->
	gen_server:cast(?MODULE,{add,From,To}).

remove(From,To) ->
	gen_server:cast(?MODULE,{remove,From,To}).

get_list(JID) ->
	gen_server:call(?MODULE,{get_list,JID}).	

get_with(JID) ->
	gen_server:call(?MODULE,{get_with,JID}).	

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
	mnesia:create_table(blacklist,[{attributes,record_info(fields,blacklist)},{disc_copies,[node()]}]),
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({get_list,JID}, _From, State) ->
	?DEBUG("get_list_input=~p",[JID]),
	F = fun()->
		Q = qlc:q([element(1,BL#blacklist.key)||BL<-mnesia:table(blacklist),element(2,BL#blacklist.key)=:=JID]),
		qlc:e(Q)
	end,
	{_,R} = mnesia:transaction(F),
	?DEBUG("get_list_result=~p",[R]),
	{reply, R, State};
handle_call({get_with,JID}, _From, State) ->
	?DEBUG("get_with_input=~p",[JID]),
	F = fun()->
		Q = qlc:q([element(1,BL#blacklist.key)||BL<-mnesia:table(blacklist),element(1,BL#blacklist.key)=:=JID]),
		qlc:e(Q)
	end,
	{_,R} = mnesia:transaction(F),
	?DEBUG("get_with_result=~p",[R]),
	{reply, R, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({add,From,To}, State) -> 
	try
		?INFO_MSG("blacklist ::> ~p ",[{add,From,To}]), 
		BL=#blacklist{key={From,To},ct=timestamp()},
		mnesia:dirty_write(blacklist,BL) 
	catch
		_:_ ->
			Err = erlang:get_stacktrace(),
			?INFO_MSG("blacklist_exception ::> ~p ; ~p",[{add,From,To},Err]) 
	end,
	{noreply, State};

handle_cast({remove,From,To}, State) -> 
	Key = {From,To},
	try
		?INFO_MSG("blacklist ::> ~p ",[{remove,From,To}]), 
		mnesia:dirty_delete(blacklist,Key) 
	catch
		_:_ ->
			Err = erlang:get_stacktrace(),
			?INFO_MSG("blacklist_exception ::> ~p ; ~p",[{remove,From,To},Err]) 
	end,
	{noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
timestamp() ->  
	{M, S, _} = os:timestamp(),
	M * 1000000 + S.
