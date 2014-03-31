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
	 user_send_packet_handler/3
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%user_send_packet(From, To, Packet) -> ok
user_send_packet_handler(From, To, Packet) ->
	?INFO_MSG("###### my_hookhandler ::::> user_send_packet_handler ~p",[liangchuan_debug]),
	%% TODO 在每个包里，寻找群聊的包，并过滤出来
	gen_server:cast(?MODULE,{group_chat_filter,From,To,Packet}),
	gen_server:cast(aa_log,{store,Packet}),
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
	    	?INFO_MSG("#### user_send_packet Host=~p~n",[Host])
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
