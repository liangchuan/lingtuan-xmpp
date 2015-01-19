-module(aa_offline_mod).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% 离线消息对象
-record(offline_msg, {us, timestamp, expire, from, to, packet}).
-define(EXPIRE,60*60*24*7).

%% ====================================================================
%% API functions
%% ====================================================================

-export([
	 start_link/0,
	 offline_message_hook_handler/3,
	 offline_message_hook_handler/4,
	 sm_register_connection_hook_handler/3,
	 sm_remove_connection_hook_handler/3,
	 user_available_hook_handler/1
	]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

sm_register_connection_hook_handler(SID, JID, Info) -> ok.

user_available_hook_handler(JID) -> send_offline_msg(JID).

send_offline_msg(#jid{user=User,server=Domain,resource=Resource}=JID) ->
	%% JID={jid,"cc","test.com","Smack","cc","test.com","Smack"} 
	%% {jid,User,Domain,_,_,_,_} = JID,
	%% if user=1 and is not gropuchat then ... 
	KEY = User++"@"++Domain++"/offline_msg",
	?INFO_MSG("@@@@ send_offline_msg :::> KEY=~p >>>>>>>>>>>>>>>>>>>>>>>",[KEY]),
	%% R = gen_server:call(?MODULE,{range_offline_msg,KEY}),
	{ok,R} = aa_hookhandler:ecache_cmd(["ZRANGE",KEY,"0","-1"]),
	%% TODO 这里，如果发送失败了，是需要重新发送的，但是先让他跑起来
	?INFO_MSG("@@@@ send_offline_msg :::> KEY=~p ; R.size=~p~n",[KEY,length(R)]),
	lists:foreach(fun(ID)->
		try	
			%% 如果是小秘书帐户，需要过滤hset中的key，如果有则不需要发送
			SEND = case User =:= "1" of
				true ->
					HK = User++"@"++Domain++"/"++Resource,
					case aa_hookhandler:ecache_cmd( ["HGET",HK,ID] ) of 
						{ok,undefined} ->
							true;
						_ ->
							false
					end;
				false ->
					true
			end,
			case SEND of 
				true ->			
		    		%% case gen_server:call(?MODULE,{ecache_cmd,["GET",ID]}) of
		    		case aa_hookhandler:ecache_cmd( ["GET",ID] ) of 
		    		    {ok,Obj} when erlang:is_binary(Obj) ->
		  			      {FF,TT,PP} = erlang:binary_to_term(Obj),
		  			      Rtn = case ejabberd_router:route(FF, TT, PP) of
		  					    ok -> ok; 
		  					    Err -> "Error: "++Err
		  				    end,
		  			      ?INFO_MSG("@ SEND :::::> KEY=~p; ID=~p ",[KEY,ID]);
		    		    Other ->	
		  			      %% ZREM_R = gen_server:call(?MODULE,{ecache_cmd,["ZREM",KEY,ID]}),
		  			      CMD = ["ZREM",KEY,ID],
		  				  ZREM_R = aa_hookhandler:ecache_cmd(CMD),
		  			      ?INFO_MSG("@ SEND [DEL]::::> KEY=~p; ID=~p; ERR=~p; ZREM_R=~p",[KEY,ID,Other,ZREM_R])	
		    		end;
				_ ->
					skip
			end
		catch
		    E:I ->
		        ?INFO_MSG("~p ; ~p",[E,I])	
		end,
		ok
    end,R),	
	?INFO_MSG("@@@@ send_offline_message ::>KEY=~p  <<<<<<<<<<<<<<<<<",[KEY]),
	ok.


sm_remove_connection_hook_handler(SID, JID, Info) -> ok.

%% 离线消息事件
%% 保存离线消息
offline_message_hook_handler(save,#jid{user=FromUser}=From, #jid{user=User,server=Domain}=To, Packet) ->
	Type = xml:get_tag_attr_s("type", Packet),
	ID = xml:get_tag_attr_s("id", Packet),
	IS_GROUP = aa_group_chat:is_group_chat(To),
	if IS_GROUP==false,FromUser=/="messageack",User=/="messageack",Type=/="error",Type=/="groupchat",Type=/="headline" ->
			SYNCID = ID++"@"++Domain,
			%% Time = xml:get_tag_attr_s("msgTime", Packet),
			%% ?INFO_MSG("ERROR++++++++++++++++ Time=~p;~n~nPacket=~p",[Time,Packet]),
			%% {ok,TimeStamp} = getTime(Time),
			%% TODO 7天以后过期
			%% Exp = ?EXPIRE+TimeStamp,
			KEY = User++"@"++Domain++"/offline_msg",
			?INFO_MSG("::::store_offline_msg::::>type=~p;KEY=~p",[Type,KEY]),
			%% gen_server:call(?MODULE,{store_offline_msg,KEY,SYNCID});
			%% 20141115: 防止因为排队产生瓶颈
			SCORE = integer_to_list(index_score()),
			CMD = ["ZADD",KEY,SCORE,SYNCID],
			aa_hookhandler:ecache_cmd(CMD);
	   true ->
			ok
	end.
	
offline_message_hook_handler(#jid{user=FromUser}=From, #jid{user=User,server=Domain}=To, Packet) ->
	try
		?INFO_MSG("offline_message_hook_handler ==> ",[]),
		Log_node = ejabberd_config:get_local_option({log_node,Domain}),
		apns_push(From,To,Packet,Log_node),
		offline_message_hook_handler(save,#jid{user=FromUser}=From, #jid{user=User,server=Domain}=To, Packet) 
		%% {apns_push:atom,jid:string,id:string,msgtype:string,msg:string}
	catch 
		C:E ->
			Err = erlang:get_stacktrace(),
			?ERROR_MSG("offline_message_hook_error=> C=~p ; E=~p ; Err=~p",[C,E,Err])
	end,
	ok.


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, { ecache_node, ecache_mod=ecache_main, ecache_fun=cmd,log_node }).

init([]) ->
	?INFO_MSG("INIT_START_OFFLINE_MOD >>>>>>>>>>>>>>>>>>>>>>>> ~p",[liangchuan_debug]),  
	lists:foreach(
	  fun(Host) ->
			  ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, offline_message_hook_handler, 40),
			  ejabberd_hooks:add(sm_remove_connection_hook, Host, ?MODULE, sm_remove_connection_hook_handler, 40),
			  ejabberd_hooks:add(sm_register_connection_hook, Host, ?MODULE, sm_register_connection_hook_handler, 60),
			  ejabberd_hooks:add(user_available_hook, Host, ?MODULE, user_available_hook_handler, 40)
	  end, ?MYHOSTS),
	?INFO_MSG("INIT_END_OFFLINE_MOD <<<<<<<<<<<<<<<<<<<<<<<<< ~p",[liangchuan_debug]),
	Conn = conn_ecache_node(),
	{ok,_,Node} = Conn,
	
	[Domain|_] = ?MYHOSTS,
	Log_node = ejabberd_config:get_local_option({log_node,Domain}),
	{ok, #state{ecache_node=Node,log_node=Log_node}}.

handle_cast(Msg, State) -> {noreply, State}.
handle_call({clear_offline_msg,KEY,ID},_From, State) -> 
	?DEBUG("##### clear_offline_msg at ack :::> Key=~p ; ID=~p",[KEY,ID]),
	R = ecache_cmd(["ZREM",KEY,ID],State),
	{reply,R,State};
handle_call({range_offline_msg,KEY},_From, State) -> 
	%% 倒序: zrevrange
	%% 正序: zrange
	R = ecache_cmd(["ZRANGE",KEY,"0","-1"],State),
	?DEBUG("##### range_offline_msg :::> Key=~p ; R=~p",[KEY,R]),
	{reply,R,State};
handle_call({store_offline_msg,KEY,ID},_From, State) -> 
	SCORE = integer_to_list(index_score()),
	?DEBUG("##### store_offline_msg :::> Key=~p ; Time=~p ; ID=~p",[KEY,SCORE,ID]),
	R = ecache_cmd(["ZADD",KEY,SCORE,ID],State),
	?DEBUG("##### store_offline_msg :::> R=~p",[R]),
	{reply,R,State};
handle_call({ecache_cmd,Cmd},_From, State) -> 
	?DEBUG("##### ecache_cmd_on_offline_mod :::> Cmd=~p",[Cmd]),
	{reply,ecache_cmd(Cmd,State),State}.

ecache_cmd(Cmd,#state{ecache_node=Node,ecache_mod=Mod,ecache_fun=Fun}=State) ->
	{ok,R} = rpc:call(Node,Mod,Fun,[Cmd]),
	%% ?DEBUG("==== ecache_cmd ===> Cmd=~p ; R=~p",[Cmd,R]),
	R.

handle_info(Info, State) -> {noreply, State}.
terminate(Reason, State) -> ok.
code_change(OldVsn, State, Extra) -> {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
timestamp() ->  
	{M, S, _} = os:timestamp(),
	M * 1000000 + S.
getTime(Time) when is_binary(Time) ->
	{ok,erlang:binary_to_integer(Time)};
getTime(Time) when is_list(Time) ->
	{ok,erlang:list_to_integer(Time)};
getTime([]) ->
	{ok,timestamp()}.
index_score()-> {M,S,T} = now(),  M*1000000000000+S*1000000+T.
conn_ecache_node() ->
	try
		[Domain|_] = ?MYHOSTS, 
		N = ejabberd_config:get_local_option({ecache_node,Domain}),
		{ok,net_adm:ping(N),N}
	catch
		E:I ->
			Err = erlang:get_stacktrace(),
			log4erl:error("error ::::> E=~p ; I=~p~n Error=~p",[E,I,Err]),
			{error,E,I}
	end.

apns_push(#jid{user=FU,server=FS,resource=FR}=From,#jid{user=TU,server=TS,resource=TR}=To,Packet,Node)->
	?INFO_MSG("apns_push ::::> From=~p ; To=~p ; Node=~p ; Packet=~p",[From,To,Node,Packet]),
	F = case is_list(FR) of
		true ->
			case length(FR)>0 of
				true ->
					FU++"@"++FS++"/"++FR;
				_ ->
					FU++"@"++FS
			end;
		false ->
			FU++"@"++FS
	end,
	T = case is_list(TR) of
		true ->
			case length(TR)>0 of
				true ->
					TU++"@"++TS++"/"++TR;
				_ ->
					TU++"@"++TS
			end;
		false ->
			TU++"@"++TS
	end,

	case Packet of 
		{xmlelement,"message",Attr,_} -> 
			D = dict:from_list(Attr),
			ID      = case dict:is_key("id",D) of true-> dict:fetch("id",D); false-> "" end,
			%% From    = case dict:is_key("from",D) of true-> dict:fetch("from",D); false-> "" end,
			%% To      = case dict:is_key("to",D) of true-> dict:fetch("to",D); false-> "" end,
			MsgType = case dict:is_key("msgtype",D) of true-> dict:fetch("msgtype",D); false-> "" end,
			Mask = case dict:is_key("mask",D) of true-> dict:fetch("mask",D); false-> "0" end,
			Msg     = erlang:list_to_binary(aa_log:get_text_message_from_packet(Packet)),

			%% #jid{user=User,server=Domain} = jlib:string_to_jid(xml:get_tag_attr_s("to", Packet)),
			#jid{user=User,server=Domain} = To, 
			KEY = User++"@"++Domain++"/offline_msg",
			%% R = gen_server:call(?MODULE,{range_offline_msg,KEY}),
			{ok,R} = aa_hookhandler:ecache_cmd(["ZRANGE",KEY,"0","-1"]),
			%% {ok,<<"none">>}
			%% 20141211 : 修正过滤掉无效的 key 
			%% 20141223 : 这里过滤是画蛇添足，没有实际意义只会增加服务器负担
			%% R0 = lists:map(fun(K0)->
			%% 	case catch aa_hookhandler:ecache_cmd(["TYPE",K0]) of
			%% 		{ok,<<"none">>} ->
			%% 			skip;
			%% 		Obj0 ->
			%% 			Obj0
			%% 	end
			%% end,R),
			%% R1 = [ X0 || X0 <- R0 , X0 =/= skip ],
			%% B = length(R1),	
			B = length(R),	
			Message = {apns_push,ID,F,T,MsgType,Msg,integer_to_list(B)},
			case MsgType of
				"msgStatus" ->
					?DEBUG("apns_push_skip msgtype=msgStatus ; id=~p",[ID]),
					skip;
				_ ->
					case Mask of
						"0" ->
							case net_adm:ping(Node) of
								pang ->
									?INFO_MSG("push_apn_by_log_pang ::::> ~p",[Message]),
									Message;
								pong ->
									[JSON] = aa_log:get_text_message_from_packet(Packet),	
									{ok,JO,_} = rfc4627:decode(erlang:list_to_binary(JSON)),
									%% 150110: get_mask_user接口有些变动,增加一个push参数，如果这个参数为1就推送否则不推	
									%% 需求来自 戚银
									%% only for msgtype=normalchat 
									case MsgType =:= "normalchat" of 
										true ->
											case rfc4627:get_field(JO,"push") of 
												{ok,<<"1">>} ->
													?INFO_MSG("push_apn_by_log_pong ::::> ~p",[Message]),
													{logbox,Node}!Message;
												Other ->
													?INFO_MSG("[v.150110] apns_push_skip push=~p ; id=~p",[Other,ID]) 
											end;
										_ ->
											?INFO_MSG("push_apn_by_log_pong ::::> ~p",[Message]),
											{logbox,Node}!Message 
									end
							end;
						_ ->
							?DEBUG("apns_push_skip mask=1 ; id=~p",[ID]),
							skip
					end
			end;
		_ ->
			?DEBUG("apns_push_skip not_message",[]),
			skip
	end.
