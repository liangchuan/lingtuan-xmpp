-module(aa_http).

-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

%% API
-export([start_link/0]).

-define(Port,5380).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).
-record(success,{success=true,entity}).

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

handle_http(Req) ->
	gen_server:call(?MODULE,{handle_http,Req}).

http_response({S,Req}) ->
	try
		{M,S1,SS1} = now(),
		SN_T = erlang:integer_to_list(M*1000000000000+S1*1000000+SS1),
		Res = {obj,[{sn,list_to_binary(SN_T)},{success,S#success.success},{entity,S#success.entity}]},
		?DEBUG("Res_obj=~p",[Res]),
		J = rfc4627:encode(Res),
		?DEBUG("Res_json=~p",[J]),
		Req:ok([{"Content-Type", "text/json"}], "~s", [J]) 
	catch
		_:_->
			Err = erlang:get_stacktrace(),
			Req:ok([{"Content-Type", "text/json"}], "~s", ["{\"success\":false,\"entity\":\""++Err++"\"}"]) 
	end.

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
	misultin:start_link([{port, ?Port}, {loop, fun(Req) -> handle_http(Req) end}]),
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
%% http://localhost:5380/?body={"method":"process_counter"}
handle_call({handle_http,Req}, _From, State) ->
	Reply = try
		Method = Req:get(method),
		Args = case Method of
			'GET' ->
				Req:parse_qs();
			'POST' ->
				Req:parse_post()
		end,
		?DEBUG("http_ARGS ::> ~n~p",[Args]),	 
		Body = case Args of 
			       [{"body",P0}]->
				       	P0;
			       [{"body",[],P1}] ->
					P1;
			       O->
					?INFO_MSG("error_args ::>~n~p",[Args]), 
					""
		end,
		[{"body",Body}] = Args,
		{ok,Obj,_Re} = rfc4627:decode(Body),
		?INFO_MSG("http ::> body=~p",[Body]),	 
		{ok,M} = rfc4627:get_field(Obj, "method"),
		S = case rfc4627:get_field(Obj, "service") of {ok,SS} -> binary_to_list(SS); _-> none end,
		case binary_to_list(M) of 
			"process_counter" ->
				Counter = aa_process_counter:process_counter(),
				http_response({#success{success=true,entity=Counter},Req});
			"add" when S =:= "blacklist" ->
				?INFO_MSG("http blacklist.add ::> ~p",[Args]),
				try
					{ok,P} = rfc4627:get_field(Obj, "params"),
					{ok,From} = rfc4627:get_field(P, "from"),
					{ok,To} = rfc4627:get_field(P, "to"),
					aa_blacklist:add(From,To),
					http_response({#success{success=true,entity=list_to_binary("ok")},Req}) 
				catch 
					_:_->
						Err = erlang:get_stacktrace(),
						http_response({#success{success=false,entity=list_to_binary(Err)},Req}) 
				end;
			"remove" when S =:= "blacklist" ->
				?INFO_MSG("http blacklist.remove ::> ~p",[Args]),
				try
					{ok,P} = rfc4627:get_field(Obj, "params"),
					{ok,From} = rfc4627:get_field(P, "from"),
					{ok,To} = rfc4627:get_field(P, "to"),
					aa_blacklist:remove(From,To),
					http_response({#success{success=true,entity=list_to_binary("ok")},Req}) 
				catch 
					_:_->
						Err = erlang:get_stacktrace(),
						http_response({#success{success=false,entity=list_to_binary(Err)},Req}) 
				end;
			"get" when S =:= "blacklist" ->
				?INFO_MSG("http blacklist.get ::> ~p",[Args]),
				try
					{ok,P} = rfc4627:get_field(Obj, "params"),
					{ok,JID} = rfc4627:get_field(P, "jid"),
					BList = aa_blacklist:get_list(JID),
					http_response({#success{success=true,entity=BList},Req}) 
				catch 
					_:_->
						Err = erlang:get_stacktrace(),
						http_response({#success{success=false,entity=list_to_binary(Err)},Req}) 
				end;
			"with" when S =:= "blacklist" ->
				?INFO_MSG("http blacklist.with ::> ~p",[Args]),
				try
					{ok,P} = rfc4627:get_field(Obj, "params"),
					{ok,JID} = rfc4627:get_field(P, "jid"),
					BList = aa_blacklist:get_with(JID),
					http_response({#success{success=true,entity=BList},Req}) 
				catch 
					_:_->
						Err = erlang:get_stacktrace(),
						http_response({#success{success=false,entity=list_to_binary(Err)},Req}) 
				end;
			"reload" when S =:= "group_user" ->
				?INFO_MSG("http group_user.reload ::> ~p",[Args]),
				try
					{ok,P} = rfc4627:get_field(Obj, "params"),
					{ok,GID} = rfc4627:get_field(P, "gid"),
					{ok,Domain} = rfc4627:get_field(P, "domain"),
					Domain_str = case is_binary(Domain) of true -> binary_to_list(Domain); _-> Domain end,
					case aa_group_chat:reload_group_user(Domain_str,GID) of 
						{ok,_,_,_,_} ->
							http_response({#success{success=true,entity=list_to_binary("ok")},Req});
						_ ->
							http_response({#success{success=false,entity=list_to_binary("callback_error")},Req}) 
					end
				catch 
					_:_->
						Err = erlang:get_stacktrace(),
						?ERROR_MSG("group_user.reload.error ~p",[Err]),
						http_response({#success{success=false,entity=exception},Req}) 
				end;
			_ ->
				http_response({#success{success=false,entity=list_to_binary("method undifine")},Req})
		end
	catch
		_:Reason -> 
			?INFO_MSG("==== aa_http_normal ====~p",[Reason]) 
	end,
	{reply,Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(_Msg, State) ->
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
handle_info(_Info, State) ->
    {noreply, State}.

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

