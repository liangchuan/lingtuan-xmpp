-module(aa_group_chat).

-include("ejabberd.hrl").
-include("jlib.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
	start_link/0,
	route_group_msg/3,
	is_group_chat/1
]).

-record(state, {}).

start() ->
	aa_group_chat_sup:start_child().

stop(Pid)->
	gen_server:cast(Pid,stop).

start_link() ->
	gen_server:start_link(?MODULE,[],[]).

route_group_msg(From,To,Packet)->
	{ok,Pid} = start(),
	?DEBUG("###### route_group_msg_001 ::::> {From,To,Packet}=~p",[{From,To,Packet}]),
	gen_server:call(Pid,{route_group_msg,From,To,Packet}),
	stop(Pid).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
init([]) ->
	{ok,#state{}}.

handle_call({route_group_msg,#jid{server=Domain}=From,#jid{user=GroupId}=To,Packet}, _From, State) ->
	%% TODO CALL WEBAPP , GET ROSTER OF GROUP;
	%% jid in roster list
	%% -record(jid, {user, server, resource, luser, lserver, lresource}).
	UserList = ["g0","g1","g2","g3","g4","g5"],
	Roster = lists:map(fun(User)-> #jid{user="g0",server=Domain,luser="g0",lserver=Domain,resource=[],lresource=[]} end,UserList),
	?DEBUG("###### route_group_msg 002 :::> GroupId=~p ; Roster=~p",[GroupId,Roster]),
	lists:foreach(fun(Target)-> route_msg(From,Target,Packet,GroupId) end,Roster),	
	{reply,[],State}.

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info({cmd,Args},State)->
	{noreply,State}.

terminate(Reason, State) ->
    ok.
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
route_msg(From,#jid{user=User,server=Domain}=To,Packet,GroupId) ->
	{X,E,Attr,Body} = Packet,
	?DEBUG("##### route_group_msg_003 param :::> {User,Domain,GroupId}=~p",[{User,Domain,GroupId}]),
	RAttr0 = lists:map(fun({K,V})-> case K=:="to" of true-> {K,User++"@"++Domain}; _-> {K,V} end end,Attr),
	RAttr1 = lists:append(RAttr0,[{"groupid",GroupId}]),
	RPacket = {X,E,RAttr1,Body},
	?DEBUG("###### route_group_msg 003 input :::> {From,To,RPacket}=~p",[{From,To,RPacket}]),
	case ejabberd_router:route(From, To, RPacket) of
		ok ->
			?DEBUG("###### route_group_msg 003 OK :::> {From,To,RPacket}=~p",[{From,To,RPacket}]),
			{ok,ok};
		Err ->
			?DEBUG("###### route_group_msg 003 ERR=~p :::> {From,To,RPacket}=~p",[Err,{From,To,RPacket}]),
			{error,Err}
	end.

is_group_chat(#jid{server=Domain}=To)->
	DomainTokens = string:tokens(Domain,"."),
	case length(DomainTokens) > 2 of 
		true ->
			[G|L] = DomainTokens,
			G=:="group";
		_ ->
			false
	end.
