-module(aa_log).

-behaviour(gen_server).


-include("ejabberd.hrl").
-include("jlib.hrl").

%% API
-export([start_link/0,store/1,get_text_message_from_packet/1]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {node}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

store(Packet) ->
	gen_server:cast(aa_log,{store,Packet}).

init([]) ->
	[Domain|_] = ?MYHOSTS,
	N = ejabberd_config:get_local_option({log_node,Domain}),
	{ok, #state{node=N}}.

get_text_message_from_packet( Packet )->
	{xmlelement,"message",_,Message } = Packet,
	List = feach_message(Message,[]),
	?DEBUG("~p ==== ~p ",[liangc_debug_offline_message,List]),
	List.


feach_message([Element|Message],List) ->
	case Element of
		{xmlelement,"body",_,_} ->
			feach_message(Message,[get_text_message_form_packet_result(Element)|List]);
		_ ->
			feach_message(Message,List)
	end;
feach_message([],List) ->
	List.


get_text_message_form_packet_result( Body )->
       {xmlelement,"body",_,List} = Body, 
       Res = lists:map(fun({_,V})-> binary_to_list(V) end,List),
       ResultMessage = binary_to_list(list_to_binary(Res)),
       ResultMessage.

log(Packet,N) ->
	try
		case Packet of 
			{xmlelement,"message",Attr,_} -> 
				D = dict:from_list(Attr),
				ID      = case dict:is_key("id",D) of true-> dict:fetch("id",D); false-> "" end,
				From    = case dict:is_key("from",D) of true-> dict:fetch("from",D); false-> "" end,
				To      = case dict:is_key("to",D) of true-> dict:fetch("to",D); false-> "" end,
				MsgType = case dict:is_key("msgtype",D) of true-> dict:fetch("msgtype",D); false-> "" end,
				Msg     = erlang:list_to_binary(get_text_message_from_packet(Packet)),
				Message = {ID,From,To,MsgType,Msg},
				case net_adm:ping(N) of
					pang ->
						?INFO_MSG("write_log ::::> ~p",[Message]),
						Message;
					pong ->
						%% 2014-11-07 : 这里的消息没有过滤，太多造成hiddenNode 频繁死机
						%% 				关闭此功能
						%% {logbox,N}!Message
						?INFO_MSG("write_log ::::> ~p",[Message]) 
				end;
			_ ->
				skip
		end
	catch
		E:I ->
			Err = erlang:get_stacktrace(),
			?ERROR_MSG("write_log_error ::::> E=~p ; I=~p~n Error=~p",[E,I,Err]),
			{error,E,I}
	end.



handle_cast({store,Packet},#state{node=N}=State) ->
	log(Packet,N),
	{noreply, State}.


handle_call(_Request, _From, State) -> Reply = ok, {reply, Reply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

