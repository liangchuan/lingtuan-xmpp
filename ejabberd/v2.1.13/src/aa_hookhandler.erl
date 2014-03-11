-module(aa_hookhandler).
-behaviour(gen_server).

-include("ejabberd.hrl").
-include("jlib.hrl").

-define(HTTP_HEAD,"application/x-www-form-urlencoded").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================

-export([
	 start_link/0,
	 user_send_packet_handler/3,
	 offline_message_hook_handler/3,
	 roster_in_subscription_handler/6
	 %roster_out_subscription_handler/6
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).



%% Message 有时是长度大于1的列表，所以这里要遍历
%% 如果列表中有多个要提取的关键字，我就把他们组合成一个 List
%% 大部分时间 List 只有一个元素
feach_message([Element|Message],List) ->
	case Element of 
		{xmlelement,"body",_,_} ->
			feach_message(Message,[get_text_message_form_packet_result(Element)|List]);
		_ ->
			feach_message(Message,List)
	end;
feach_message([],List) ->
	List.

%% 获取消息包中的文本消息，用于离线消息推送服务
get_text_message_from_packet( Packet )->
	{xmlelement,"message",_,Message } = Packet,
	%% Message 结构不固定，需要遍历
	List = feach_message(Message,[]),
	?INFO_MSG("~p ==== ~p ",[liangc_debug_offline_message,List]),
	List.

%% 获取消息包中的文本消息，用于离线消息推送服务
get_text_message_form_packet_result( Body )->
	{xmlelement,"body",_,[{xmlcdata,MessageBody}]} = Body,
	ResultMessage = binary_to_list(MessageBody),
	ResultMessage.	

%% 离线消息处理器
%% 钩子回调
offline_message_hook_handler(From, To, Packet) ->
	?INFO_MSG("FFFFFFFFFFFFFFFFF===From=~p~nTo=~p~nPacket=~p~n",[From, To, Packet]),
	{xmlelement,"message",Header,_ } = Packet,
	%%这里只推送 msgtype=normalchat 的消息，以下是判断
	lists:foreach(
		fun(K) ->
			{Key,Value}=K,
			case Key of 
				"msgtype" -> 
					case Value of 
						"normalchat" ->
							send_offline_message(From ,To ,Packet );
						_ ->
							?INFO_MSG("FFFFFFFFFFFFFFFFF=NOT_SEND=From=~p~nTo=~p~nPacket=~p~n",[From, To, Packet])
					end;
				_ ->
					ok
			end
		end,Header),
	ok.

%% 将 Packet 中的 Text 消息 Post 到指定的 Http 服务
%% IOS 消息推送功能
send_offline_message(From ,To ,Packet )->
	{jid,FromUser,Domain,_,_,_,_} = From ,	
	{jid,ToUser,_,_,_,_,_} = To ,	
	%% 取自配置文件 ejabberd.cfg
 	HTTPServer =  ejabberd_config:get_local_option({http_server,Domain}),
	%% 取自配置文件 ejabberd.cfg
 	HTTPService = ejabberd_config:get_local_option({http_server_service_client,Domain}),
	HTTPTarget = string:concat(HTTPServer,HTTPService),
	Msg = get_text_message_from_packet( Packet ),
	{Service,Method,FN,TN,MSG} = {list_to_binary("service.uri.pet_user"),list_to_binary("pushMsgApn"),list_to_binary(FromUser),list_to_binary(ToUser),list_to_binary(Msg)},
	ParamObj={obj,[ {"service",Service},{"method",Method},{"params",{obj,[{"fromname",FN},{"toname",TN},{"msg",MSG}]} } ]},
	Form = "body="++rfc4627:encode(ParamObj),
	?INFO_MSG("MMMMMMMMMMMMMMMMM===Form=~p~n",[Form]),
	case httpc:request(post,{ HTTPTarget ,[], ?HTTP_HEAD , Form },[],[] ) of   
        	{ok, {_,_,Body}} ->
 			case rfc4627:decode(Body) of
 				{ok,Obj,_Re} -> 
					case rfc4627:get_field(Obj,"success") of
						{ok,false} ->
							{ok,Entity} = rfc4627:get_field(Obj,"entity"),
							?INFO_MSG("liangc-push-msg error: ~p~n",[binary_to_list(Entity)]);
						_ ->
							false
					end;
 				_ -> 
					false
 			end ;
        	{error, Reason} ->
 			?INFO_MSG("[~ERROR~] cause ~p~n",[Reason])
     	end,
	ok.

%roster_in_subscription(Acc, User, Server, JID, SubscriptionType, Reason) -> bool()
roster_in_subscription_handler(Acc, User, Server, JID, SubscriptionType, Reason) ->
	?INFO_MSG("~n~p; Acc=~p ; User=~p~n Server=~p ; JID=~p ; SubscriptionType=~p ; Reason=~p~n ", [liangchuan_debug,Acc, User, Server, JID, SubscriptionType, Reason] ),
	{jid,ToUser,Domain,_,_,_,_}=JID,
	?INFO_MSG("XXXXXXXX===~p",[SubscriptionType]),
	case SubscriptionType of subscribed -> 
			sync_user(Domain,User,ToUser,SubscriptionType);
		unsubscribed ->
			sync_user(Domain,User,ToUser,SubscriptionType);
		_ -> 
			ok
	end,
	true.

%% 好友同步
sync_user(Domain,FromUser,ToUser,SType) ->
	HTTPServer =  ejabberd_config:get_local_option({http_server,Domain}),
	HTTPService = ejabberd_config:get_local_option({http_server_service_client,Domain}),
	HTTPTarget = string:concat(HTTPServer,HTTPService),
	{Service,Method,Channel} = {list_to_binary("service.uri.pet_user"),list_to_binary("addOrRemoveFriend"),list_to_binary("9")},
	{AID,BID,ST} = {list_to_binary(FromUser),list_to_binary(ToUser),list_to_binary(atom_to_list(SType))},
	%% 2013-10-22 : 新的请求协议如下，此处不必关心，success=true 即成功
	%% INPUT {"SubscriptionType":"", "aId":"", "bId":""}
	%% OUTPUT {"success":true,"entity":"OK" }
	PostBody = {obj,[{"service",Service},{"method",Method},{"channel",Channel},{"params",{obj,[{"SubscriptionType",ST},{"aId",AID},{"bId",BID}]}}]},	
	JsonParam = rfc4627:encode(PostBody),
	ParamBody = "body="++JsonParam,
	URL = HTTPServer++HTTPService++"?"++ParamBody,
	?INFO_MSG("~p: ~p~n ",[liangchuan_debug,URL]),
	Form = lists:concat([ParamBody]),
	case httpc:request(post,{ HTTPTarget, [], ?HTTP_HEAD, Form },[],[] ) of   
        	{ok, {_,_,Body}} ->
			case rfc4627:decode(Body) of
				{ok , Obj , _Re } ->
					%% 请求发送出去以后，如果返回 success=false 那么记录一个异常日志就可以了，这个方法无论如何都要返回 ok	
					case rfc4627:get_field(Obj,"success") of
						{ok,false} ->	
							{ok,Entity} = rfc4627:get_field(Obj,"entity"),
							?INFO_MSG("liangc-sync-user error: ~p~n",[binary_to_list(Entity)]);
						_ ->
							false
					end;
				_ -> 
					false
			end ;
        	{error, Reason} ->
			?INFO_MSG("[~ERROR~] cause ~p~n",[Reason])
    	end,
	?INFO_MSG("[--OKOKOKOK--] ~p was done.~n",[addOrRemoveFriend]),
	ok.

%roster_out_subscription(Acc, User, Server, JID, SubscriptionType, Reason) -> bool()
%roster_out_subscription_handler(Acc, User, Server, JID, SubscriptionType, Reason) ->
%	true.

%user_send_packet(From, To, Packet) -> ok
user_send_packet_handler(From, To, Packet) ->
	?INFO_MSG("~n************** my_hookhandler user_send_packet_handler >>>>>>>>>>>>>>>~p~n ",[liangchuan_debug]),
	?INFO_MSG("~n~pFrom=~p ; To=~p ; Packet=~p~n ", [liangchuan_debug,From, To, Packet] ),
	?INFO_MSG("~n************** my_hookhandler user_send_packet_handler <<<<<<<<<<<<<<<~p~n ",[liangchuan_debug]),
	ok.
	
%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

init([]) ->
	?INFO_MSG("INIT_START >>>>>>>>>>>>>>>>>>>>>>>> ~p",[liangchuan_debug]),  
	lists:foreach(
	  fun(Host) ->
		?INFO_MSG("#### _begin Host=~p~n",[Host]),
		ejabberd_hooks:add(user_send_packet,Host,?MODULE, user_send_packet_handler ,80),
	    ?INFO_MSG("#### user_send_packet Host=~p~n",[Host]),
		ejabberd_hooks:add(roster_in_subscription,Host,?MODULE, roster_in_subscription_handler ,90),
		?INFO_MSG("#### roster_in_subscription Host=~p~n",[Host]),
		ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, offline_message_hook_handler, 45),
		?INFO_MSG("#### offline_message_hook Host=~p~n",[Host])
		%ejabberd_hooks:add(roster_out_subscription,Host,?MODULE, roster_out_subscription_handler ,90),
		%?INFO_MSG("#### roster_out_subscription Host=~p~n",[Host])
  	  end, ?MYHOSTS),
	?INFO_MSG("INIT_END <<<<<<<<<<<<<<<<<<<<<<<<< ~p",[liangchuan_debug]),
    {ok, #state{}}.

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.
handle_cast(Msg, State) ->
    {noreply, State}.
handle_info(Info, State) ->
    {noreply, State}.
terminate(Reason, State) ->
    ok.
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
