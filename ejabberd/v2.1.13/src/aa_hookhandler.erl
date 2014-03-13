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
	 offline_message_hook_handler/3
	 %% roster_in_subscription_handler/6
	 %% roster_out_subscription_handler/6
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
	?INFO_MSG("OFFLINE_MESSAGE_HOOK ::::> From=~p~nTo=~p~nPacket=~p~n",[From, To, Packet]),
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
							?INFO_MSG("OFFLINE_MESSAGE_HOOK ::::> =NOT_SEND=From=~p~nTo=~p~nPacket=~p~n",[From, To, Packet])
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
	{Service,Method,FN,TN,MSG} = {
				list_to_binary("service.uri.pet_user"),
				list_to_binary("pushMsgApn"),
				list_to_binary(FromUser),
				list_to_binary(ToUser),
				list_to_binary(Msg)
	},
	ParamObj={obj,[ {"service",Service},{"method",Method},{"params",{obj,[{"fromname",FN},{"toname",TN},{"msg",MSG}]} } ]},
	Form = "body="++rfc4627:encode(ParamObj),
	?INFO_MSG("=== MMMMMM ===Form=~p~n",[Form]),
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

%user_send_packet(From, To, Packet) -> ok
user_send_packet_handler(From, To, Packet) ->
	?INFO_MSG("###### my_hookhandler ::::> user_send_packet_handler ~p",[liangchuan_debug]),
	%% TODO 在每个包里，寻找群聊的包，并过滤出来
	gen_server:cast(?MODULE,{group_chat_filter,From,To,Packet}),
	ok.
	


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

init([]) ->
	?INFO_MSG("INIT_START >>>> ~p",[liangchuan_debug]),  
	lists:foreach(
	  fun(Host) ->
		?INFO_MSG("#### _begin Host=~p~n",[Host]),
		ejabberd_hooks:add(user_send_packet,Host,?MODULE, user_send_packet_handler ,80),
	    	?INFO_MSG("#### user_send_packet Host=~p~n",[Host]),
		%% ejabberd_hooks:add(roster_in_subscription,Host,?MODULE, roster_in_subscription_handler ,90),
		%% ?INFO_MSG("#### roster_in_subscription Host=~p~n",[Host]),
		ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, offline_message_hook_handler, 45),
		?INFO_MSG("#### offline_message_hook Host=~p~n",[Host])
		%ejabberd_hooks:add(roster_out_subscription,Host,?MODULE, roster_out_subscription_handler ,90),
		%?INFO_MSG("#### roster_out_subscription Host=~p~n",[Host])
  	  end, ?MYHOSTS),
	?INFO_MSG("INIT_END <<<< ~p",[liangchuan_debug]),
    {ok, #state{}}.

handle_call(Request, From, State) ->
	{reply, [], State}.
handle_cast({group_chat_filter,From,#jid{server=Domain}=To,Packet}, State) ->
	%% -record(jid, {user, server, resource, luser, lserver, lresource}).
	case aa_group_chat:is_group_chat(To) of 
		true ->
			?DEBUG("###### send_group_chat_msg ###### From=~p ; Domain=~p",[From,Domain]),
			aa_group_chat:route_group_msg(From,To,Packet);
		false ->
			skip
	end,
	{noreply, State}.

handle_info(Info, State) -> {noreply, State}.
terminate(Reason, State) -> ok.
code_change(OldVsn, State, Extra) -> {ok, State}.
%% ====================================================================
%% Internal functions
%% ====================================================================
