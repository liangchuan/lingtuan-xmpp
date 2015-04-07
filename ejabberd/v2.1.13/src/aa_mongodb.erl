%% @author Administrator
%% @doc @todo Add description to aa_mongodb.


-module(aa_mongodb).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(OPERATE_USERLIST,"operate_userlist").

-define(SYSN_TIME,30*1000).			%%同步时间间隔

-define(MONGO_POOL,mongo_pool).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,
		save_mongo/3
		]).

start_link()->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {userlist}).

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
	UserList = 
		try
			lists:filter(fun(Uid)-> erlang:is_integer(Uid) end, aa_mongo_senter:read_opt_uidlist())
		catch
			_:_->
				[]
		end,
	erlang:send_after(?SYSN_TIME, ?MODULE, read_mongouser),			%%同步mongo数据库中的运营账户
	?WARNING_MSG("sysn_mongo state:~p",[UserList]),
    {ok, #state{userlist = UserList}}.


%%异步存库
save_mongo(From, To, Packet)->
    case Packet of 
    	{_,"message",_Attr,_} ->
			gen_server:cast(?MODULE, {save_mongo,From, To,Packet});
		_->
			skip
	end.

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
    Reply = error,
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
handle_cast({save_mongo,#jid{luser = Fromstr} = _FromJid, #jid{luser = Tostr} = ToJid,Packet},State)->
	
	Mid = xml:get_tag_attr_s("id", Packet),
	Type = xml:get_tag_attr_s("type", Packet),
	MsgType = xml:get_tag_attr_s("msgtype", Packet),
	OldMsgTime = xml:get_tag_attr_s("msgTime", Packet),
	if
		OldMsgTime =/= ""->
			MsgTime = OldMsgTime;
		true->
			{M,S,SS} = now(), 
			MsgTime = lists:sublist(erlang:integer_to_list(M*1000000000000+S*1000000+SS),1,13)
	end,
	try
		case MsgType of
			"normalchat"->
				From = erlang:list_to_integer(Fromstr),
				To = erlang:list_to_integer(Tostr),
				Result =  lists:member(To, State#state.userlist),
				FResult = lists:member(From, State#state.userlist),
				if
					Result =:= false,FResult =:= false->
						skip;
					true->
						[JSON] = aa_log:get_text_message_from_packet(Packet),
						Body = normal_deel_json_to_bson(JSON),
						DbMsg = {
								 id,list_to_binary(Mid),
								 from,From,
								 to,To,
								 type,list_to_binary(Type),
								 msgtype,list_to_binary(MsgType),
								 msgTime,erlang:list_to_integer(MsgTime),
								 body,Body,
								 isread,[]
								 },
						aa_mongo_senter:sysn_write([DbMsg])
				end;
			"groupchat"->
					From = erlang:list_to_integer(Fromstr),
					To = erlang:list_to_integer(Tostr),
					case aa_group_chat:is_group_chat(ToJid) of
						false->
							Result =  lists:member(To, State#state.userlist);
						true->
							Result = lists:member(From, State#state.userlist)
					end,
					if
						Result =:= false->
							skip;
						true->
							[JSON] = aa_log:get_text_message_from_packet(Packet),
							Body = normal_deel_json_to_bson(JSON),
							DbMsg = {
									 id,list_to_binary(Mid),
									 from,From,
									 to,To,
									 type,list_to_binary(Type),
									 msgtype,list_to_binary(MsgType),
									 msgTime,erlang:list_to_integer(MsgTime),
									 body,Body,
									 isread,[]
									 },
							aa_mongo_senter:sysn_write([DbMsg])
					end;
			"super_groupchat"->
						From = erlang:list_to_integer(Fromstr),
						To = erlang:list_to_integer(Tostr),
						case aa_group_chat:is_group_chat(ToJid) of
							false->
								Result =  lists:member(To, State#state.userlist);
							true->
								Result = lists:member(From, State#state.userlist)
						end,
						if
							Result =:= false->
								skip;
							true->
								[JSON] = aa_log:get_text_message_from_packet(Packet),
								Body = normal_deel_json_to_bson(JSON),
								DbMsg = {
										 id,list_to_binary(Mid),
										 from,From,
										 to,To,
										 type,list_to_binary(Type),
										 msgtype,list_to_binary(MsgType),
										 msgTime,erlang:list_to_integer(MsgTime),
										 body,Body,
										 isread,[]
										 },
								aa_mongo_senter:sysn_write([DbMsg])
						end;
			"system"->
				To = erlang:list_to_integer(Tostr),
				From = erlang:list_to_integer(Fromstr),
				case aa_group_chat:is_group_chat(ToJid) of
					false->
						Result =  lists:member(To, State#state.userlist);
					true->
						Result = lists:member(From, State#state.userlist)
				end,
				if
					Result=:= false->
						skip;
					true->
						[JSON] = aa_log:get_text_message_from_packet(Packet),
						Body = normal_deel_json_to_bson(JSON),
						DbMsg = {
								 id,list_to_binary(Mid),
								 from,From,
								 to,To,
								 type,list_to_binary(Type),
								 msgtype,list_to_binary(MsgType),
								 msgTime,erlang:list_to_integer(MsgTime),
								 body,Body,
								 isread,[]
								 },
						aa_mongo_senter:sysn_write([DbMsg])
				end;
			_->
				skip
		end
	catch
		E:R->
			?ERROR_MSG("Error happen at ~p :save_mongo()reason:~p",[?MODULE,{E,R,erlang:get_stacktrace()}])
	end,
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
handle_info(read_mongouser,State)->
	?INFO_MSG("sysn_mongo state:~p",[State]),
	UserList = lists:filter(fun(Uid)-> erlang:is_integer(Uid) end, aa_mongo_senter:read_opt_uidlist()),
	NewState = State#state{userlist = UserList},
	erlang:send_after(?SYSN_TIME, ?MODULE, read_mongouser),
	{noreply, NewState};

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

normal_deel_json_to_bson(JSON)->
	{ok,{obj,Ljson},_} = rfc4627:decode(erlang:list_to_binary(JSON)),
	Ncover = lists:keydelete("cover", 1, Ljson),
	Tsonlist = 
		case lists:keyfind("groupmember", 1, Ncover) of
			{_,Grm}->
				NewGrm = 
				lists:map(fun(G)->
							{_,Gd} = G,
							list_to_tuple(lists:append(lists:map(fun({K,V})->[erlang:list_to_atom(K),V] end, Gd)))
						  end, Grm),
				lists:keyreplace("groupmember", 1, Ncover, {"groupmember",NewGrm});
			_->
				Ncover
		end,
	list_to_tuple(lists:append(lists:map(fun({K,V})->[erlang:list_to_atom(K),V] end, Tsonlist))).
