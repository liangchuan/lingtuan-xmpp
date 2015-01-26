-module(aa_hookhandler).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(HTTP_HEAD,"application/x-www-form-urlencoded").
-define(TIME_OUT,1000*15).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,ecache_cmd/1]).
%% -export([handle_call/2, handle_cast/1]).

%% ====================================================================
%% API functions
%% ====================================================================

-export([
	 start_link/0,
	 user_send_packet_handler/3
]).

%% -record(state, { ecache_node, ecache_mod=ecache_main, ecache_fun=cmd log_node }).
-record(state, {}). 
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%user_send_packet(From, To, Packet) -> ok
user_send_packet_handler(#jid{user=FUser,server=FDomain}=From, #jid{user=TUser,server=TDomain}=To, Packet) ->
	try
	    case Packet of 
	    	{_,"message",Attr,_} ->
			    D = dict:from_list(Attr),
			    MT = case dict:is_key("msgtype",D) of true-> dict:fetch("msgtype",D); _-> "" end,
				case MT of
					"msgStatus" ->
						%% gen_server:cast(?MODULE,{group_chat_filter,From,To,Packet}),
						%% gen_server:cast(aa_log,{store,Packet});
						%% 20141223 : 为了提高效率，这个地方必须放弃 otp
						handle_cast({group_chat_filter,From,To,Packet},#state{});
					_ ->
						?INFO_MSG("###### my_hookhandler ::::> user_send_packet_handler ~p",[liangchuan_debug]),
						BlacklistKey = {list_to_binary(FUser++"@"++FDomain),list_to_binary(TUser++"@"++TDomain)}, 
						?DEBUG("BlacklistKey=~p",[BlacklistKey]),
						case length(mnesia:dirty_read(blacklist,BlacklistKey)) of
							0 ->
								%% gen_server:cast(?MODULE,{group_chat_filter,From,To,Packet}),
								%% gen_server:cast(aa_log,{store,Packet});
								%% 20141223 : 为了提高效率，这个地方必须放弃 otp
								handle_cast({group_chat_filter,From,To,Packet},#state{});
							_ ->
								%% gen_server:cast(?MODULE,{server_ack,From,To,Packet}),
								%% 20141223 : 为了提高效率，这个地方必须放弃 otp
								handle_cast({server_ack,From,To,Packet},#state{}),
								MSG_ID = xml:get_tag_attr_s("id", Packet),
								?INFO_MSG("discard_message ::> Blacklist_key=~p ; MSG_ID=~p",[BlacklistKey,MSG_ID])
						end 
				end;
			_ ->
				skip
		end 
	catch 
		_:_->
			Err = erlang:get_stacktrace(),
			?ERROR_MSG("user_send_packet_handler_error ~p",[Err])
	end,
	ok.
	


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(dmsg,{mid,pid}).

init([]) ->
	?INFO_MSG("INIT_START >>>> ~p",[liangchuan_debug]),  
	lists:foreach(
	  fun(Host) ->
		?INFO_MSG("#### _begin Host=~p~n",[Host]),
		ejabberd_hooks:add(user_send_packet,Host,?MODULE, user_send_packet_handler ,70),
	    	?INFO_MSG("#### user_send_packet Host=~p~n",[Host])
  	  end, ?MYHOSTS),
	?INFO_MSG("INIT_END <<<< ~p",[liangchuan_debug]),
	Conn = conn_ecache_node(),
	?INFO_MSG("INIT_END <<<<<<<<<<<<<<<<<<<<<<<<< Conn=~p",[Conn]),
	%% {ok,_,Node} = Conn,
	mnesia:create_table(dmsg,[{attributes,record_info(fields,dmsg)},{ram_copies,[node()]}]),
	%% {ok, #state{ecache_node=Node}}.
	%% 20141223 : ecache_node 不再存储在 state 中, 瓶颈
	{ok, #state{}}.


%% 20141223 : 其实这两个方法已经被废弃了,用更高效的方式实现了
handle_call({ecache_cmd,Cmd}, _F, State) ->
	?DEBUG("==== ecache_cmd ===> Cmd=~p",[Cmd]),
	R = rpc:call(ecache_node(),ecache_main,cmd,[Cmd]),
	{reply, R, State};
handle_call({sync_packet,K,From,To,Packet}, _F, State) ->
	{M,S,SS} = now(), 
	MsgTime = lists:sublist(erlang:integer_to_list(M*1000000000000+S*1000000+SS),1,13),
        {Tag,E,Attr,Body} = Packet,
        RAttr0 = lists:map(fun({K,V})-> case K of "msgTime" -> skip; _-> {K,V} end end,Attr),
        RAttr1 = lists:append([X||X<-RAttr0,X=/=skip],[{"msgTime",MsgTime}]),
        RPacket = {Tag,E,RAttr1,Body},
        V = term_to_binary({From,To,RPacket}),
        ?DEBUG("==== sync_packet ===> insert K=~p~nV=~p",[K,V]),
        Cmd = ["PSETEX",K,integer_to_list(1000*60*60*24*7),V],
		R = rpc:call(ecache_node(),ecache_main,cmd,[Cmd]),
        aa_offline_mod:offline_message_hook_handler(save,From,To,RPacket),
        {reply, R, State}.

handle_cast({group_chat_filter,From,To,Packet}, State) ->
	try
		filter_cast({From,To,Packet,true}, State) 
	catch 
		_:_ ->
			Err = erlang:get_stacktrace(),
			?ERROR_MSG("group_chat_filter_error ~p",[Err])
	end,
	{noreply, State};
handle_cast({group_chat_filter,From,To,Packet,SACK}, State) ->
	try
		filter_cast({From,To,Packet,SACK}, State) 
	catch 
		_:_ ->
			Err = erlang:get_stacktrace(),
			?ERROR_MSG("group_chat_filter_error ~p",[Err])
	end,
	{noreply, State};
handle_cast({server_ack,_From,_To,_Packet}, State) ->
	%% server_ack(_From,_To,_Packet,State).
	{noreply, State};
handle_cast(_, State) -> {noreply, State}.

filter_cast({#jid{server=Domain}=From,#jid{user=TUser}=To,Packet,SACK}, State) ->
	%% -record(jid, {user, server, resource, luser, lserver, lresource}).
	[_,E|_] = tuple_to_list(Packet),
	case E of 
		"message" -> 
			case SACK of true -> server_ack(From,To,Packet,State); _-> skip end,
			case aa_group_chat:is_group_chat(To) of 
				true ->
					?DEBUG("###### send_group_chat_msg ###### From=~p ; Domain=~p",[From,Domain]),
        			{_,"message",Attr,_} = Packet,
        			D = dict:from_list(Attr),
        			MT = case dict:is_key("msgtype",D) of true-> dict:fetch("msgtype",D); _-> "" end,
					case MT=:="msgStatus" of
						true ->
							?DEBUG("###### ack_group_chat_msg ###### Packet=~p",[Packet]),
							message_handler(From,To,Packet,State);
						_->
							%% groupchat and msgtype=system body is json format
							case MT=:="system" of
								true ->
									[JSON] = aa_log:get_text_message_from_packet(Packet),	
									?DEBUG("SYSTEM ::::> JSON=~p",[JSON]),
									{ok,JO,_} = rfc4627:decode(erlang:list_to_binary(JSON)),
									?DEBUG("SYSTEM ::::> JO=~p",[JO]),
									case rfc4627:get_field(JO,"type") of 
										not_found ->
											skip;
										{ok,<<"3">>} ->
											TK = "toinvitedlist",
											%% {ok,ToList} = rfc4627:get_field(JO,TK),
											case rfc4627:get_field(JO,TK) of
												not_found ->
													skip;
												{ok,ToList} ->
													?DEBUG("T3T3T3T3 ::::> ToList=~p",[ToList]),
													{obj,TL3} = JO,
													TL3_1=lists:map(fun({K3,V3})-> case K3=:=TK of true->skip;false->{K3,V3} end end,TL3),
													TL3_2=[X||X<-TL3_1,X=/=skip],
													JO_1 = {obj,TL3_2},
													?DEBUG("T3T3T3T3 ::::> JO_1=~p",[JO_1]), 
													J4B = list_to_binary(rfc4627:encode(JO_1)),
													?DEBUG("T3T3T3T3 ::::> J4B=~p",[J4B]), 
													lists:foreach(fun(To3) ->
														UID = binary_to_list(To3),
														JID3=#jid{user=UID,server=Domain,luser=UID,
																  lserver=Domain,resource=[],lresource=[]},
														route_3(From,JID3,Packet,J4B)	
													end,ToList) 
											end;
										{ok,<<"7">>} ->
											TK = "applylist",
											%% {ok,ToList} = rfc4627:get_field(JO,TK),
											case rfc4627:get_field(JO,TK) of
												not_found ->
													skip;
												{ok,ToList} ->
													?DEBUG("T7T7T7T7 ::::> ToList=~p",[ToList]),
													{obj,TL3} = JO,
													TL3_1=lists:map(fun({K3,V3})-> case K3=:=TK of true->skip;false->{K3,V3} end end,TL3),
													TL3_2=[X||X<-TL3_1,X=/=skip],
													JO_1 = {obj,TL3_2},
													?DEBUG("T7T7T7T7 ::::> JO_1=~p",[JO_1]), 
													J4B = list_to_binary(rfc4627:encode(JO_1)),
													?DEBUG("T7T7T7T7 ::::> J4B=~p",[J4B]), 
													lists:foreach(fun(To3) ->
														UID = binary_to_list(To3),
														JID3=#jid{user=UID,server=Domain,luser=UID,
																  lserver=Domain,resource=[],lresource=[]},
														route_3(From,JID3,Packet,J4B)	
													end,ToList) 
											end;
										{ok,<<"8">>} ->
											TK = "applylist",
											%% {ok,ToList} = rfc4627:get_field(JO,TK),
											case rfc4627:get_field(JO,TK) of
												not_found ->
													skip;
												{ok,ToList} ->
													?DEBUG("T8T8T8T8 ::::> ToList=~p",[ToList]),
													{obj,TL3} = JO,
													TL3_1=lists:map(fun({K3,V3})-> case K3=:=TK of true->skip;false->{K3,V3} end end,TL3),
													TL3_2=[X||X<-TL3_1,X=/=skip],
													JO_1 = {obj,TL3_2},
													?DEBUG("T8T8T8T8 ::::> JO_1=~p",[JO_1]), 
													J4B = list_to_binary(rfc4627:encode(JO_1)),
													?DEBUG("T8T8T8T8 ::::> J4B=~p",[J4B]), 
													lists:foreach(fun(To3) ->
														UID = binary_to_list(To3),
														JID3=#jid{user=UID,server=Domain,luser=UID,
																  lserver=Domain,resource=[],lresource=[]},
														route_3(From,JID3,Packet,J4B)	
													end,ToList) 
											end;
										{ok,<<"13">>} ->
											TK = "grouplist",
											%% {ok,ToList} = rfc4627:get_field(JO,TK),
											case rfc4627:get_field(JO,TK) of
												not_found ->
													skip;
												{ok,ToList} ->
													?DEBUG("T13T13T13T13 ::::> ToList=~p",[ToList]),
													{obj,TL3} = JO,
													TL3_1=lists:map(fun({K3,V3})-> case K3=:=TK of true->skip;false->{K3,V3} end end,TL3),
													TL3_2=[X||X<-TL3_1,X=/=skip],
													JO_1 = {obj,TL3_2},
													?DEBUG("T13T13T13T13 ::::> JO_1=~p",[JO_1]), 
													J4B = list_to_binary(rfc4627:encode(JO_1)),
													?DEBUG("T13T13T13T13 ::::> J4B=~p",[J4B]), 
													lists:foreach(fun(To3) ->
														UID = binary_to_list(To3),
														JID3=#jid{user=UID,server=Domain,luser=UID,
																  lserver=Domain,resource=[],lresource=[]},
														route_3(To,JID3,Packet,J4B)	
													end,ToList) 
											end;
										{ok,<<"15">>} ->
											TK = "grouplist",
											%% {ok,ToList} = rfc4627:get_field(JO,TK),
											case rfc4627:get_field(JO,TK) of
												not_found ->
													skip;
												{ok,ToList} ->
													?DEBUG("T15T15T15T15 ::::> ToList=~p",[ToList]),
													{obj,TL3} = JO,
													TL3_1=lists:map(fun({K3,V3})-> case K3=:=TK of true->skip;false->{K3,V3} end end,TL3),
													TL3_2=[X||X<-TL3_1,X=/=skip],
													JO_1 = {obj,TL3_2},
													?DEBUG("T15T15T15T15 ::::> JO_1=~p",[JO_1]), 
													J4B = list_to_binary(rfc4627:encode(JO_1)),
													?DEBUG("T15T15T15T15 ::::> J4B=~p",[J4B]), 
													lists:foreach(fun(To3) ->
														UID = binary_to_list(To3),
														JID3=#jid{user=UID,server=Domain,luser=UID,
																  lserver=Domain,resource=[],lresource=[]},
														route_3(To,JID3,Packet,J4B)	
													end,ToList) 
											end;
										{ok,<<"20">>} ->
											%% 140823 : add by liangc 
											%% 宇庭新需求：社交圈新动态消息（ejabber需要处理）      apns推送内容（无） 
											TK = "grouplist",
											%% {ok,ToList} = rfc4627:get_field(JO,TK),
											case rfc4627:get_field(JO,TK) of
												not_found ->
													skip;
												{ok,ToList} ->
													?DEBUG("T20T20 ::::> ToList=~p",[ToList]),
													{obj,TL3} = JO,
													TL3_1=lists:map(fun({K3,V3})-> case K3=:=TK of true->skip;false->{K3,V3} end end,TL3),
													TL3_2=[X||X<-TL3_1,X=/=skip],
													JO_1 = {obj,TL3_2},
													?DEBUG("T20T20 ::::> JO_1=~p",[JO_1]), 
													J4B = list_to_binary(rfc4627:encode(JO_1)),
													?DEBUG("T20T20 ::::> J4B=~p",[J4B]), 
													lists:foreach(fun(To3) ->
														UID = binary_to_list(To3),
														JID3=#jid{user=UID,server=Domain,luser=UID,
																  lserver=Domain,resource=[],lresource=[]},
														route_3(To,JID3,Packet,J4B)	
													end,ToList) 
											end;
										{ok,<<"21">>} ->
											%% 140925 : add by liangc 
											%% 宇庭新需求：
											%% ejabber服务器需要给applylist列表发送的消息格式（ejabber转发时去掉applylist）
											TK = "applylist",
											%% {ok,ToList} = rfc4627:get_field(JO,TK),
											case rfc4627:get_field(JO,TK) of
												not_found ->
													skip;
												{ok,ToList} ->
													?DEBUG("T21T21T21 ::::> ToList=~p",[ToList]),
													{obj,TL3} = JO,
													TL3_1=lists:map(fun({K3,V3})-> case K3=:=TK of true->skip;false->{K3,V3} end end,TL3),
													TL3_2=[X||X<-TL3_1,X=/=skip],
													JO_1 = {obj,TL3_2},
													?DEBUG("T21T21T21 ::::> JO_1=~p",[JO_1]), 
													J4B = list_to_binary(rfc4627:encode(JO_1)),
													?DEBUG("T21T21T21 ::::> J4B=~p",[J4B]), 
													lists:foreach(fun(To3) ->
														UID = binary_to_list(To3),
														JID3=#jid{user=UID,server=Domain,luser=UID,
																  lserver=Domain,resource=[],lresource=[]},
														route_3(From,JID3,Packet,J4B)	
													end,ToList) 
											end;
										{ok,<<"22">>} ->
											%% 2015-01-25
											%% 有人被邀请加入多人会话或者有人被踢出的消息（ejabber需要处理）groupmember 
											%% 最多传递5个人的数据（用来显示头像拼接成多人对话图片）      apns推送内容（无）        
											%% content字段： 如果是有人被邀请加入讨论组：xx,yy,zz被邀请加入群组     如果是有人被踢出：xx被管理员移出群组
											TK = "grouplist",
											%% {ok,ToList} = rfc4627:get_field(JO,TK),
											case rfc4627:get_field(JO,TK) of
												not_found ->
													skip;
												{ok,ToList} ->
													?DEBUG("T22T22T22T22 ::::> ToList=~p",[ToList]),
													{obj,TL3} = JO,
													TL3_1=lists:map(fun({K3,V3})-> case K3=:=TK of true->skip;false->{K3,V3} end end,TL3),
													TL3_2=[X||X<-TL3_1,X=/=skip],
													JO_1 = {obj,TL3_2},
													?DEBUG("T15T15T15T15 ::::> JO_1=~p",[JO_1]), 
													J4B = list_to_binary(rfc4627:encode(JO_1)),
													?DEBUG("T22T22T22T22 ::::> J4B=~p",[J4B]), 
													lists:foreach(fun(To3) ->
														UID = binary_to_list(To3),
														JID3=#jid{user=UID,server=Domain,luser=UID,
																  lserver=Domain,resource=[],lresource=[]},
														route_3(To,JID3,Packet,J4B)	
													end,ToList) 
											end;
										{ok,<<"23">>} ->
											%% 2015-01-25
											%% 好友推荐的消息（ejabber需要处理） apns推送内容（无）  
											TK = "grouplist",
											case rfc4627:get_field(JO,TK) of
												not_found ->
													skip;
												{ok,ToList} ->
													?DEBUG("T22T22T22T22 ::::> ToList=~p",[ToList]),
													{obj,TL3} = JO,
													TL3_1=lists:map(fun({K3,V3})-> case K3=:=TK of true->skip;false->{K3,V3} end end,TL3),
													TL3_2=[X||X<-TL3_1,X=/=skip],
													JO_1 = {obj,TL3_2},
													?DEBUG("T15T15T15T15 ::::> JO_1=~p",[JO_1]), 
													J4B = list_to_binary(rfc4627:encode(JO_1)),
													?DEBUG("T22T22T22T22 ::::> J4B=~p",[J4B]), 
													lists:foreach(fun(To3) ->
														UID = binary_to_list(To3),
														JID3=#jid{user=UID,server=Domain,luser=UID,
																  lserver=Domain,resource=[],lresource=[]},
														route_3(To,JID3,Packet,J4B)	
													end,ToList) 
											end;
										_ ->
											case TUser =:= "0" of
												true ->
													skip;
												false ->
													aa_group_chat:route_group_msg(From,To,Packet) 
											end
									end;
								_ ->
									case TUser =:= "0" of
										true ->
											skip;
										false ->
											aa_group_chat:route_group_msg(From,To,Packet) 
									end
							end 
					end;
				false ->
					message_handler(From,To,Packet,State)
			end;
		_ ->
			skip
	end,
	{noreply, State}.

handle_info(Info, State) -> {noreply, State}.
terminate(Reason, State) -> ok.
code_change(OldVsn, State, Extra) -> {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
ecache_node()->
	{ok,_,Node} = conn_ecache_node(),
	Node.
conn_ecache_node() ->
	try
		[Domain|_] = ?MYHOSTS,
		N = ejabberd_config:get_local_option({ecache_node,Domain}),
		{ok,net_adm:ping(N),N}
	catch
		E:I ->
		Err = erlang:get_stacktrace(),
		?ERROR_MSG("error ::::> E=~p ; I=~p~n Error=~p",[E,I,Err]),
		{error,E,I}
	end.

server_ack(#jid{user=FU,server=FD}=From,To,Packet,State) ->
	ok.
server_ack(old,#jid{user=FU,server=FD}=From,To,Packet,State) ->
	Domain = FD,
        {_,"message",Attr,_} = Packet,
        D = dict:from_list(Attr),
        T = dict:fetch("type", D),
        MT = case dict:is_key("msgtype",D) of true-> dict:fetch("msgtype",D); _-> "" end,
        SRC_ID_STR = case dict:is_key("id", D) of true -> dict:fetch("id", D); _ -> "" end,
        ACK_FROM = case ejabberd_config:get_local_option({ack_from ,Domain}) of true -> true; _ -> false end,
        if ACK_FROM and ( ( MT=:="normalchat" ) or ( MT=:="groupchat") ) ->
                case dict:is_key("from", D) of
                        true ->
								{M8,S8,T8} = now(),
								ID8 = integer_to_list(M8*1000000000000+S8*1000000+T8),
                                Attributes = [
                                        {"id",ID8},
                                        {"to",dict:fetch("from", D)},
                                        {"from","messageack@"++Domain},
                                        {"type","normal"},
                                        {"msgtype",""},
                                        {"action","ack"}
                                ],
                                Child = [{xmlelement, "body", [], [
                                                {xmlcdata, list_to_binary("{'src_id':'"++SRC_ID_STR++"','received':'true'}")}
                                ]}],
                                Answer = {xmlelement, "message", Attributes , Child},
                                FF = jlib:string_to_jid(xml:get_tag_attr_s("from", Answer)),
                                TT = jlib:string_to_jid(xml:get_tag_attr_s("to", Answer)),
                                ?DEBUG("Answer ::::> FF=~p ; TT=~p ; P=~p ", [FF,TT,Answer] ),
                                case catch ejabberd_router:route(FF, TT, Answer) of
                                        ok -> ?DEBUG("Answer ::::> ~p ", [ok] );
                                        _ERROR -> ?DEBUG("Answer ::::> error=~p ", [_ERROR] )
                                end,
                                answer;
                        _ ->
                                ?DEBUG("~p", [skip_01] ),
                                skip
                end;
          true ->
                ok
        end.


message_handler(#jid{user=FU,server=FD,resource=FR}=From,#jid{server=TD}=To,Packet,State) ->
	%% TODO 特殊处理
	%% <message id="xxxxx" from="yy@group.test.com" to="123456@test.com" type="normal" msgtype=“system”>
	%% TODO 处理 message 消息，进来的都是 message
	{_,"message",Attr,_} = Packet, 
	?DEBUG("Attr=~p", [Attr] ), 
	D = dict:from_list(Attr), 
	T = dict:fetch("type", D), 
	MT = case dict:is_key("msgtype",D) of true-> dict:fetch("msgtype",D); _-> "" end,
	Domain = case MT of 
		"system" -> 
			%% SF = dict:fetch("from", D), 
			[C|_] = string:tokens(FD,"."),
			case C of 
				"group" ->
					TD;
				_ ->
					FD
			end;
		_ ->
			FD
	end,
	SRC_ID_STR = case dict:is_key("id", D) of true -> dict:fetch("id", D); _ -> "" end,
	?DEBUG("SRC_ID_STR=~p ; other=~p", [SRC_ID_STR,{From,To,Domain}] ),
	ACK_FROM = case ejabberd_config:get_local_option({ack_from ,Domain}) of true -> true; _ -> false end,
	?DEBUG("ack_from=~p ; Domain=~p ; T=~p ; MT=~p",[ACK_FROM,{FD,TD},T,MT]),
	SYNCID = SRC_ID_STR++"@"++Domain, 
	if ACK_FROM,MT=/=[],MT=/="msgStatus",FU=/="messageack" -> 
		   ?DEBUG("==> SYNC_RES start => ID=~p",[SRC_ID_STR]), 
		   %% SyncRes = handle_call({sync_packet,SYNCID,From,To,Packet},[],State), 
		   %% 20141115 : 防止在本模块排队产生瓶颈
		   SyncRes = sync_packet(SYNCID,From,To,Packet), 
		   ?DEBUG("==> SYNC_RES new => ~p ; ID=~p",[SyncRes,SRC_ID_STR]);
		   %% ack_task({new,SYNCID,From,To,Packet}); 
	   ACK_FROM,MT=:="msgStatus" -> 
 			case FU =:= "1" of 
				false ->	
		   			KK = FU++"@"++FD++"/offline_msg", 
		   			ack_sync(SYNCID,State,0),
		   			ecache_cmd(["ZREM",KK,SYNCID]), 
		   			?WARNING_MSG("[v.141223] ==> SYNC_RES ack => ACK_USER=~p ; ACK_ID=~p",[KK,SYNCID]); 
				true ->
					%% 不能删除，而是增加一个 hset 
		   			HK = FU++"@"++FD++"/"++FR, 
					{M2,S2,SS2} = now(), 
					HV = lists:sublist(erlang:integer_to_list(M2*1000000000000+S2*1000000+SS2),1,13),
					ecache_cmd(["HSET",HK,SYNCID,HV]),
		   			?WARNING_MSG("[v.150103] ==> SYNC_RES ack_user_1 => ACK_ID=~p ; V=~p",[SYNCID,HV])
			end;
	   true -> 
		   skip 
	end, 
	ok.

ack_sync( SYNCID,State , N ) when N =< 6 ->
	Cmd = ["DEL",SYNCID],
	case rpc:call(ecache_node(),ecache_main,cmd,[Cmd]) of  
		{ok,<<"0">>} ->
			timer:sleep(500),
			?WARNING_MSG("REDEL ACK_ID=~p ; N=~p",[SYNCID,N]),
			ack_sync(SYNCID,State,N+1);
		_ ->
			ok
	end;
ack_sync(SYNCID,_State,N) when N > 6 ->
	?WARNING_MSG("SYNC_RES ack_id_notfound ; ACK_ID=~p",[SYNCID]).
			
	

  
ack_task({new,ID,From,To,Packet})-> 
	TPid = erlang:spawn(fun()-> ack_task(ID,From,To,Packet) end), 
	mnesia:dirty_write(dmsg,#dmsg{mid=ID,pid=TPid});
ack_task({ack,ID})->
	ack_task({do,ack,ID});
ack_task({offline,ID})-> 
	ack_task({do,offline,ID});
ack_task({do,M,ID})-> 
	try [{_,_,ResendPid}] = mnesia:dirty_read(dmsg,ID), ResendPid!M catch _:_-> ack_err end.
ack_task(ID,From,To,Packet)-> 
	?INFO_MSG("ACK_TASK_~p ::::> START.",[ID]),
	receive 
		offline-> 
			mnesia:dirty_delete(dmsg,ID), 
			?INFO_MSG("ACK_TASK_~p ::::> OFFLINE.",[ID]); 
		ack -> 
			mnesia:dirty_delete(dmsg,ID), 
			?INFO_MSG("ACK_TASK_~p ::::> ACK.",[ID]) 
	after ?TIME_OUT -> 
		?INFO_MSG("ACK_TASK_~p ::::> AFTER",[ID]), 
		mnesia:dirty_delete(dmsg,ID), 
        %% aa_offline_mod:offline_message_hook_handler(From,To,Packet),
		{xmlelement,"message",Header,_ } = Packet,
		D = dict:from_list(Header),
		V = dict:fetch("msgtype", D),
		case V of "msgStatus" -> ok;
			_ -> ack_task({offline,ID})  
		end	
	end.  


route_3(From,#jid{user=User,server=Server}=To,Packet,J4B)->
	B4J = binary_to_list(J4B),
	{ok,Obj,_} = rfc4627:decode(B4J),
	DT = case rfc4627:get_field(Obj,"type") of 
		{ok,NN} ->
			?WARNING_MSG("aaaaaaaaaaaaaaa type=~p",[NN]),
			NN;
		_ ->
			skip
	end,
	{X,E,Attr,_} = Packet,
	RAttr0 = lists:map(fun({K,V})-> 
		case K of 
			"id" ->
				{A,B,C} = now(),
				UUID = integer_to_list(A)++integer_to_list(B)++integer_to_list(C),	
				case DT of
					<<"13">> ->
						?WARNING_MSG("dddddddddddddd dt=~p ; uuid=~p",[DT,UUID]);
					<<"15">> ->
						?WARNING_MSG("dddddddddddddd dt=~p ; uuid=~p",[DT,UUID]);
					_ ->
						skip
				end,
				{K,UUID};
			"to" -> {K,User++"@"++Server};
			_-> {K,V} 
		end 
	end,Attr),
	Body = [{xmlelement,"body",[],[{xmlcdata,J4B}]}],
	RPacket = {X,E,RAttr0,Body},
	?DEBUG("route_3 :::> packet=~p",[RPacket]),
	%% 20141223 : 不管发成发不成，都得存起来
	%% gen_server:cast(aa_hookhandler,{group_chat_filter,From,To,RPacket,false}),
	handle_cast({group_chat_filter,From,To,RPacket,false},#state{}),
	case ejabberd_router:route(From, To, RPacket) of
		ok ->
			{ok,ok};
		Err ->
			{error,Err}
	end.

%% 20141115 : 这里要优化一下调用缓存模块的地方
%% 将节点信息保存在上下文里，而不是 aa_hookhandler 进程里
%% aa_hookhandler 进程就是瓶颈
ecache_cmd(Cmd) ->
	?DEBUG("==== function__ecache_cmd ===> Cmd=~p",[Cmd]),
	Node = case application:get_env(ecache,node) of 
		{ok,N0} ->
			N0;
		_ ->
			[Domain|_] = ?MYHOSTS,
			N = ejabberd_config:get_local_option({ecache_node,Domain}),
			net_adm:ping(N),
			N
	end,
	case catch rpc:call(Node,ecache_main,cmd,[Cmd]) of 
		{'EXIT',Err} ->
			net_adm:ping(Node),
			?ERROR_MSG("ecache_cmd_exception____~p",[Err]);
		Rtn ->
			Rtn
	end.
			
sync_packet(K,From,#jid{user=TU}=To,Packet) ->
	{M,S,SS} = now(), 
	MsgTime = lists:sublist(erlang:integer_to_list(M*1000000000000+S*1000000+SS),1,13),
        {Tag,E,Attr,Body} = Packet,
        RAttr0 = lists:map(fun({K,V})-> case K of "msgTime" -> skip; _-> {K,V} end end,Attr),
        RAttr1 = lists:append([X||X<-RAttr0,X=/=skip],[{"msgTime",MsgTime}]),
        RPacket = {Tag,E,RAttr1,Body},
        V = term_to_binary({From,To,RPacket}),
        ?DEBUG("==== sync_packet ===> insert K=~p~nV=~p",[K,V]),
		%% user＝1并且非群聊时，特殊处理，有效期设为2天
		IS_GROUP_CHAT = aa_group_chat:is_group_chat(To), 
		EX = case TU=:="1" of 
			true when IS_GROUP_CHAT=:=false ->
        		1000*60*60*24*2;
			false ->	
        		1000*60*60*24*7 
		end,
        Cmd = ["PSETEX",K,integer_to_list(EX),V],
        R = ecache_cmd(Cmd),
        aa_offline_mod:offline_message_hook_handler(save,From,To,RPacket),
		R.
