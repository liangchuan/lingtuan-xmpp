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
	Res = {obj,[{success,S#success.success},{entity,S#success.entity}]},
	J = rfc4627:encode(Res),
	Req:ok([{"Content-Type", "text/json"}], "~s", [J]).

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
		[{"body",Body}] = Args,
		{ok,Obj,_Re} = rfc4627:decode(Body),
		%%{ok,T} = rfc4627:get_field(Obj, "token"),
		{ok,M} = rfc4627:get_field(Obj, "method"),

		case binary_to_list(M) of 
			"process_counter" ->
				Counter = aa_process_counter:process_counter(),
				http_response({#success{success=true,entity=Counter},Req});
			_ ->
				http_response({#success{success=false,entity=list_to_binary("method undifine")},Req})
		end
	catch
		_:Reason -> ?INFO_MSG("==== aa_http ====~p",[Reason])
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

