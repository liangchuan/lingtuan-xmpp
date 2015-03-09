-module(aa_packet_filter).

-export([do/1,reload/3,reload_all/2]).

-define(HTTP_HEAD,"application/x-www-form-urlencoded").
-include("ejabberd.hrl").
-include("jlib.hrl").

%% 2014-10-19 : 应对 16 号的屏蔽需求
%% 需求一：
%% 是做关于普通单人聊天msgtype=“normalchat”的这种类型的消息提醒屏蔽功能，类似于当时做的拉黑功能，这个目前只做针对普通单人聊天消息的提示屏蔽功能，方案如下:
%% 针对单人聊天消息，web端会给你同步每个用户的消息提示屏蔽名单，类似于黑名单，我们发送普通msgtype=“normalchat”聊天消息时，你接收到消息以后需要拦截，判断接受者的屏蔽消息名单里是否包含发送者，如果包含则不做apns推送，并且给消息body里边增加mask字段 ，0 未屏蔽  1屏蔽   如下所示：
%% <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
%%      <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"0","content":"hello！"}
%% 	      </body>
%% 		       </message>
%% 			   这是你接收到的消息。
%% 
%% 			   <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
%% 			        <body>{"mask":"0","userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"0","content":"hello！"}
%% 					     </body>
%% 						      </message>
%% 							  这是你拦截以后转发出来的消息，增加了mask字段。
%% 
%% 							  我们客户端接收到消息以后根据mask字段来判断是否提示该消息。
							  
%% 需求二：
%% 是做关于普通单人聊天msgtype=“normalchat”的这种类型的消息关系字段显示，这个目前只做针对普通单人聊天消息的提示屏蔽功能，方案如下:
%% 
%% web接口提供一个获取两人的关系接口，XMPP每次发送聊天消息时到接口获取关系friend_log信息，然后附带在消息内发送出去，XMPP需要对关系进行缓存，关系是单向的，可以缓存10-20分钟来减少两个服务器之间的压力。
%% <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
%%      <body>{"userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"0","content":"hello！"}
%% 	      </body>
%% 		       </message>
%% 			   这是你接收到的消息。
%% 
%% 
%% 			      <message id="xxxxx" from="xx@test.com" to="yy@test.com" type="chat" msgtype=“normalchat”>
%% 				       <body>{"friend_log":"0","userid":"xx","username":"张三","userimage":"http://wwww.1.jpg","usergender":"0","type":"0","content":"hello！"}
%% 					        </body>
%% 							   </message>
%% 							   这是你拦截以后转发出来的消息，其中friend_log字段是你蓝接到消息以后添加到消息里边的关系字段  0：陌生人；1：好友；2：二度好友
%% 
%% 							   我们客户端接收到消息以后根据friend_log字段来判断关系。

do({r141016,#jid{server=Domain,user=FU},#jid{user=TU},Packet})->
	[_,E|_] = tuple_to_list(Packet),
	OrigPacket = case E of 
		"message" ->
			{X,E,Attr,_} = Packet,
			?DEBUG("Attr=~p", [Attr] ),
			D = dict:from_list(Attr),
			MT = case dict:is_key("msgtype",D) of true-> dict:fetch("msgtype",D); _-> "" end,
			%% FromBin = list_to_binary( case dict:is_key("from",D) of true-> dict:fetch("from",D); _-> "" end ),
			%% ToBin = list_to_binary( case dict:is_key("to",D) of true-> dict:fetch("to",D); _-> "" end ),
			FromBin = list_to_binary(FU++"@"++Domain), 
			ToBin = list_to_binary(TU++"@"++Domain),
			?INFO_MSG("aa_packet_filter__mt==>~p",[MT]),
			case MT =:= "normalchat" of 
				true ->
					[JSON] = aa_log:get_text_message_from_packet(Packet),	
					?INFO_MSG("aa_packet_filter__JSON==>~p",[JSON]),
					{ok,JO,_} = rfc4627:decode(erlang:list_to_binary(JSON)),
					?INFO_MSG("aa_packet_filter__JO==>~p",[JO]),
					JO_1 = set_mask(Domain,FromBin,ToBin,JO),	
					?INFO_MSG("aa_packet_filter__JO_1==>~p",[JO_1]),
					JO_2 = set_friend_log(Domain,FromBin,ToBin,JO_1),
					?INFO_MSG("aa_packet_filter__JO_2==>~p",[JO_2]),
					J4B = list_to_binary(rfc4627:encode(JO_2)),
					?INFO_MSG("aa_packet_filter__Body==>~p",[J4B]),
					Body = [{xmlelement,"body",[],[{xmlcdata,J4B}]}],
					{X,E,Attr,Body};
				_ ->
					Packet
			end;
		_ ->
			Packet	
	end,
	?INFO_MSG("aa_packet_filter__result==>~p",[OrigPacket]),
	OrigPacket.

get_jid(JIDStr) ->
	[U,DR] = string:tokens(JIDStr,"@"),
	[D|_] = string:tokens(DR,"/"),
	U++"@"++D.

set_mask(Domain,FromBin,ToBin,JO) ->
	case rfc4627:get_field(JO,"mask") of 
		{ok,_} ->
			JO;
		_ ->
			%% 先到缓存里找，如果没有则回调并初始化缓存;
			[FromStr,ToStr]	= [get_jid(binary_to_list(FromBin)),get_jid(binary_to_list(ToBin))],
			Key = "mask__"++FromStr++ToStr,
			
			%% case gen_server:call(aa_hookhandler,{ecache_cmd,["GET",Key]}) of
			%% 20141115 : 这里有瓶颈，不能排队取,特此修改
			case aa_hookhandler:ecache_cmd(["GET",Key]) of
				{ok,Bin} when erlang:is_binary(Bin) ->
					?INFO_MSG("aa_packet_filter__set_mask__on_cache key=~p ; mask=~p",[Key,Bin]),
					%% 150110: get_mask_user接口有些变动,增加一个push参数，如果这个参数为1就推送否则不推	
					%% 需求来自 戚银
					MaskPush = binary_to_list(Bin),
					[Mask,Push] = case string:tokens(MaskPush,",") of 
						[A1,A2] ->
							[A1,A2];
						[B1] ->
							[B1,"0"]
					end,
					J1 = rfc4627:set_field(JO,"mask",list_to_binary(Mask)),
					rfc4627:set_field(J1,"push",list_to_binary(Push));
				_ ->
					case call_http(Domain,<<"get_mask_user">>,FromBin,ToBin) of 
						{ok,Entity} ->	
							{ok,MaskBin} = rfc4627:get_field(Entity,"mask"),
							Mask = case is_binary(MaskBin) of 
								true ->
									binary_to_list(MaskBin);
								false ->
									MaskBin
							end,
							%% 150110: get_mask_user接口有些变动,增加一个push参数，如果这个参数为1就推送否则不推	
							%% 需求来自 戚银
							Push = case rfc4627:get_field(Entity,"push") of 
								{ok,Obj1} when is_binary(Obj1) ->
									binary_to_list(Obj1);
								{ok,Obj2} ->
									Obj2;
								_ ->
									"0"
							end,
							?INFO_MSG("aa_packet_filter__set_mask__on_http key=~p ; mask=~p ; push=~p",[Key,Mask,Push]),
							%% gen_server:call(aa_hookhandler,{ecache_cmd,["SET",Key,Mask]}),
							%% 20141115 : 这里有瓶颈，不能排队取,特此修改
							Val = Mask++","++Push,
							aa_hookhandler:ecache_cmd(["SET",Key,Val]),
							Key_idx = "mask_set__"++ToStr,
							{M1,S1,T1} = now(), 
							Scope = integer_to_list(M1*1000000000000+S1*1000000+T1),
							%% gen_server:call(aa_hookhandler,{ecache_cmd,["ZADD",Key_idx,Scope,Key]}),
							%% 20141115 : 这里有瓶颈，不能排队取,特此修改
							aa_hookhandler:ecache_cmd(["ZADD",Key_idx,Scope,Key]),
							J1 = rfc4627:set_field(JO,"mask",list_to_binary(Mask)),
							rfc4627:set_field(J1,"push",list_to_binary(Push));
						_ ->
							JO
					end
			end 
	end.	

set_friend_log(Domain,FromBin,ToBin,JO) ->
	case rfc4627:get_field(JO,"friend_log") of 
		{ok,_} ->
			JO;
		_ ->
			%% 2014-10-30 : 先到缓存里找，如果没有则回调并初始化缓存;
			[FromStr,ToStr]	= [get_jid(binary_to_list(FromBin)),get_jid(binary_to_list(ToBin))],
			Key = "friend_log__"++FromStr++ToStr,
			%% case gen_server:call(aa_hookhandler,{ecache_cmd,["GET",Key]}) of
			%% 20141115 : 这里有瓶颈，不能排队取,特此修改
			case aa_hookhandler:ecache_cmd(["GET",Key]) of
				{ok,Bin} when erlang:is_binary(Bin) ->
					?INFO_MSG("aa_packet_filter__set_friend_log__on_cache key=~p ; mask=~p",[Key,Bin]),
					rfc4627:set_field(JO,"friend_log",Bin);
				_ ->
					case call_http(Domain,<<"get_relation">>,FromBin,ToBin) of 
						{ok,Entity} ->	
							{ok,Friend_log} = rfc4627:get_field(Entity,"friend_log"),
							?INFO_MSG("aa_packet_filter__set_friend_log__on_http key=~p ; mask=~p",[Key,Friend_log]),
							%% 5 分钟过期
							%% gen_server:call(aa_hookhandler,{ecache_cmd,["PSETEX",Key,integer_to_list(1000*60*5),Friend_log]}),
							%% 20141115 : 这里有瓶颈，不能排队取,特此修改
							aa_hookhandler:ecache_cmd(["PSETEX",Key,integer_to_list(1000*60*5),Friend_log]),
							rfc4627:set_field(JO,"friend_log",Friend_log);
						_ ->
							JO
					end
			end 
	end.	


call_http(Domain,Method,FromBin,ToBin)->
	{M,S,SS} = now(),
	SN = erlang:integer_to_list(M*1000000000000+S*1000000+SS),
 	HTTPTarget =  ejabberd_config:get_local_option({http_server,Domain}),
	ParamObj = {obj,[ {"sn",list_to_binary(SN)}, {"service",<<"ejabberd">>}, {"method",Method},{"params",{obj,[{"from",FromBin},{"to",ToBin}]}}]}, 
	?INFO_MSG("aa_packet_filter__call_http__paramObj=~p ; method=~p ; domain=~p ~nhttp_url=~p",[ParamObj,Method,Domain,HTTPTarget]),
	Form = "body="++rfc4627:encode(ParamObj),
	case httpc:request(post,{ HTTPTarget ,[], ?HTTP_HEAD , Form },[],[] ) of
		{ok, {_,_,Body}} ->
			DBody = rfc4627:decode(Body),
			{_,Log,_} = DBody,
			?DEBUG("aa_packet_filter__call_http__response=~p",[Log]),
 			case DBody of
 				{ok,Obj,_Re} -> 
					case rfc4627:get_field(Obj,"success") of
						{ok,true} ->
							case rfc4627:get_field(Obj,"entity") of
								{ok,Entity} ->
									{ok,Entity};
								_ ->
									error
							end;
						_ ->
							{ok,Entity} = rfc4627:get_field(Obj,"entity"),
							?ERROR_MSG("[aa_packet_filter__call_http__success_false] sn=~p ; entity=~p",[SN,Entity]),
							{fail,Entity}
					end;
 				Error -> 
					?ERROR_MSG("[aa_packet_filter__call_http__error] sn=~p ; response=~p  ; error=~p",[SN,Body,Error]),
					{error,Error}
 			end ;
		{error, Reason} ->
			?ERROR_MSG("[aa_packet_filter__call_http__exception] sn=~p ; exception=~p",[SN,Reason]),
			{error,Reason}
	end.

reload(mask,FromStr,ToStr) ->
	[From,To] = [get_jid(FromStr),get_jid(ToStr)],		
	Key = "mask__"++From++To,
	%% gen_server:call(aa_hookhandler,{ecache_cmd,["DEL",Key]}),
	%% 20141115 : 这里有瓶颈，不能排队取,特此修改
	aa_hookhandler:ecache_cmd(["DEL",Key]),
	?INFO_MSG("reload__mask__key=~p",[Key]),
	ok;
reload(friend_log,FromStr,ToStr) ->
	[From,To] = [get_jid(FromStr),get_jid(ToStr)],		
	Key = "friend_log__"++From++To,
	%% gen_server:call(aa_hookhandler,{ecache_cmd,["DEL",Key]}),
	%% 20141115 : 这里有瓶颈，不能排队取,特此修改
	aa_hookhandler:ecache_cmd(["DEL",Key]),
	?INFO_MSG("reload__friend_log__key=~p",[Key]),
	ok.

reload_all(mask,ToStr) ->
	To = get_jid(ToStr),
	Key_idx = "mask_set__"++To,
	CMD = ["ZRANGE",Key_idx,"0","-1"],
	?INFO_MSG("reload_all_mask_cmd=~p",[CMD]),
	%% case gen_server:call(aa_hookhandler,{ecache_cmd,CMD}) of 
	%% 20141115 : 这里有瓶颈，不能排队取,特此修改
	case aa_hookhandler:ecache_cmd(CMD) of 
		{ok,Idxs} ->
			%% gen_server:call(aa_hookhandler,{ecache_cmd,["DEL",Key_idx]}),
			%% 20141115 : 这里有瓶颈，不能排队取,特此修改
			aa_hookhandler:ecache_cmd(["DEL",Key_idx]),
			?INFO_MSG("reload_all_mask_cmd=~p",[["DEL",Key_idx]]),
			lists:foreach(fun(KeyBin)->
				Key = binary_to_list(KeyBin),
				?INFO_MSG("reload_all_mask_item_cmd=~p",[["DEL",Key]]),
				%% gen_server:call(aa_hookhandler,{ecache_cmd,["DEL",Key]}) 
				%% 20141115 : 这里有瓶颈，不能排队取,特此修改
				aa_hookhandler:ecache_cmd(["DEL",Key]) 
			end,Idxs);
		_ ->
			skip
	end.		


