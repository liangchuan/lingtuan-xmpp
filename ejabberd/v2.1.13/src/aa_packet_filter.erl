-module(aa_packet_filter).

-export([do/1]).

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

do({r141016,Domain,Packet})->
	[_,E|_] = tuple_to_list(Packet),
	OrigPacket = case E of 
		"message" ->
			{X,E,Attr,_} = Packet,
			?DEBUG("Attr=~p", [Attr] ),
			D = dict:from_list(Attr),
			MT = case dict:is_key("msgtype",D) of true-> dict:fetch("msgtype",D); _-> "" end,
			FromBin = list_to_binary( case dict:is_key("from",D) of true-> dict:fetch("from",D); _-> "" end ),
			ToBin = list_to_binary( case dict:is_key("to",D) of true-> dict:fetch("to",D); _-> "" end ),
			case MT =:= "normalchat" of 
				true ->
					[JSON] = aa_log:get_text_message_from_packet(Packet),	
					{ok,JO,_} = rfc4627:decode(erlang:list_to_binary(JSON)),
					case rfc4627:get_field(JO,"type") of
						{ok,<<"0">>} ->	
							JO_1 = set_mask(Domain,FromBin,ToBin,JO),	
							JO_2 = set_friend_log(Domain,FromBin,ToBin,JO_1),
							J4B = list_to_binary(rfc4627:encode(JO_2)),
							?INFO_MSG("aa_packet_filter__Body==>~p",[J4B]),
							Body = [{xmlelement,"body",[],[{xmlcdata,J4B}]}],
							{X,E,Attr,Body};
						_ ->
							Packet
					end;
				_ ->
					Packet
			end;
		_ ->
			Packet	
	end,
	?INFO_MSG("aa_packet_filter__result==>~p",[OrigPacket]),
	OrigPacket.

set_mask(Domain,FromBin,ToBin,JO) ->
	case rfc4627:get_field(JO,"mask") of 
		{ok,_} ->
			JO;
		_ ->
			case call_http(Domain,<<"get_mask_user">>,FromBin,ToBin) of 
				{ok,Entity} ->	
					{ok,Mask} = rfc4627:get_field(Entity,"mask"),
					rfc4627:set_field(JO,"mask",Mask);
				_ ->
					JO
			end
	end.	

set_friend_log(Domain,FromBin,ToBin,JO) ->
	case rfc4627:get_field(JO,"friend_log") of 
		{ok,_} ->
			JO;
		_ ->
			case call_http(Domain,<<"get_relation">>,FromBin,ToBin) of 
				{ok,Entity} ->	
					{ok,Friend_log} = rfc4627:get_field(Entity,"friend_log"),
					rfc4627:set_field(JO,"friend_log",Friend_log);
				_ ->
					JO
			end
	end.	


call_http(Domain,Method,FromBin,ToBin)->
	{M,S,SS} = now(),
	SN = erlang:integer_to_list(M*1000000000000+S*1000000+SS),
 	HTTPTarget =  ejabberd_config:get_local_option({http_server,Domain}),
	ParamObj = {obj,[ {"sn",SN}, {"service",<<"ejabberd">>}, {"method",Method},{"params",{obj,[{"from",FromBin},{"to",ToBin}]}}]}, 
	?INFO_MSG("aa_packet_filter__call_http__paramObj=~p ; method=~p ; domain=~p",[ParamObj,Method,Domain]),
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

