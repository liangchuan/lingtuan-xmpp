-module(aa_group_chat).

-include("ejabberd.hrl").
-include("jlib.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(HTTP_HEAD,"application/x-www-form-urlencoded").

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
	case get_user_list_by_group_id(Domain,GroupId) of 
		{ok,UserList,Groupmember,Groupname,Masklist} ->
			%% -record(jid, {user, server, resource, luser, lserver, lresource}).
			Roster = lists:map(fun(User)-> 
				UID = binary_to_list(User),
				#jid{user=UID,server=Domain,luser=UID,lserver=Domain,resource=[],lresource=[]} 
			end,UserList),
			?DEBUG("###### route_group_msg 002 :::> GroupId=~p ; Roster=~p",[GroupId,Roster]),
			lists:foreach(fun(Target)-> route_msg(From,Target,Packet,GroupId,Groupmember,Groupname,Masklist) end,Roster);
		Err ->
			error
	end,	
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
get_user_list_by_group_id(Domain,GroupId)->
	?DEBUG("###### get_user_list_by_group_id :::> GroupId=~p",[GroupId]),
 	HTTPTarget =  ejabberd_config:get_local_option({http_server,Domain}),
	{Service,Method,GID,SN} = {
			list_to_binary("ejabberd"),
			list_to_binary("getUserList"),
			list_to_binary(GroupId),
			list_to_binary(os:cmd("uuidgen")--"\n")
	},
	ParamObj={obj,[ {"sn",SN},{"service",Service},{"method",Method},{"params",{obj,[{"groupId",GID}]} } ]},
	Form = "body="++rfc4627:encode(ParamObj),
	?DEBUG("###### get_user_list_by_group_id :::> HTTP_TARGET=~p ; request=~p",[HTTPTarget,Form]),
	case httpc:request(post,{ HTTPTarget ,[], ?HTTP_HEAD , Form },[],[] ) of   
        	{ok, {_,_,Body}} ->
			DBody = rfc4627:decode(Body),
			{_,Log,_} = DBody,
			?DEBUG("###### get_user_list_by_group_id_response=~p",[Body]),
			?DEBUG("###### get_user_list_by_group_id_decode=~p",[DBody]),
			?DEBUG("###### get_user_list_by_group_id_log=~p",[Log]),
 			case DBody of
 				{ok,Obj,_Re} -> 
					case rfc4627:get_field(Obj,"success") of
						{ok,true} ->
							{ok,Entity} = rfc4627:get_field(Obj,"entity"),
							?DEBUG("###### success=true get_user_list_by_group_id :::> entity=~p",[Entity]),
							{ok,UserList} = rfc4627:get_field(Entity,"userlist"),
							{ok,Groupmember} = rfc4627:get_field(Entity,"groupmember"),
							{ok,Masklist} = rfc4627:get_field(Entity,"masklist"),
							{ok,Groupname} = rfc4627:get_field(Entity,"groupname"),
							{ok,UserList,Groupmember,Groupname,Masklist};
						_ ->
							{ok,Entity} = rfc4627:get_field(Obj,"entity"),
							?DEBUG("###### success=false get_user_list_by_group_id :::> entity=~p",[Entity]),
							{fail,Entity}
					end;
 				Error -> 
					?DEBUG("###### get_user_list_by_group_id_error=~p",[Error]),
					{error,Error}
 			end ;
        	{error, Reason} ->
 			?INFO_MSG("[~ERROR~] cause ~p~n",[Reason]),
			{error,Reason}
     	end.

route_msg(#jid{user=FromUser}=From,#jid{user=User,server=Domain}=To,Packet,GroupId,Groupmember,Groupname,Masklist) ->
	case FromUser=/=User of
		true->
			{X,E,Attr,Body} = Packet,
			?DEBUG("##### route_group_msg_003 param :::> {User,Domain,GroupId}=~p",[{User,Domain,GroupId}]),
       		D = dict:from_list(Attr),
			{M,S,SS} = now(),
			MsgTime = lists:sublist(erlang:integer_to_list(M*1000000000000+S*1000000+SS),1,13),
       		MT = case dict:is_key("msgtype",D) of 
				true-> 
					case dict:fetch("msgtype",D) of
						"system" ->
							"system";
						_ ->
							"groupchat"
					end;
				_-> "groupchat" 
			end,
			RAttr0 = lists:map(fun({K,V})-> 
				case K of 
					"id" -> {K,os:cmd("uuidgen")--"\n"};
					"to" -> {K,User++"@"++Domain};
					"msgtype" -> {K,MT};	
					"msgTime" -> skip;
					_-> {K,V} 
				end 
			end,Attr),
			Mask = case lists:member(User,Masklist) of true -> 1; false-> 0 end,
			RAttr1 = lists:append(RAttr0,[{"groupid",GroupId}]),
			RAttr2 = lists:append(RAttr1,[{"mask",Mask}]),
			RAttr3 = lists:append([X||X<-RAttr2,X=/=skip],[{"msgTime",MsgTime}]),

			%% TODO Groupmember
			[JSON] = aa_log:get_text_message_from_packet(Packet),	
			?DEBUG("GROUP ::::> JSON=~p",[JSON]),
			{ok,JO,_} = rfc4627:decode(erlang:list_to_binary(JSON)),
			?DEBUG("GROUP ::::> JO=~p",[JO]),
			J1 = rfc4627:set_field(JO,"groupmember",Groupmember),
			J2 = rfc4627:set_field(J1,"groupname",Groupname),
			RJ0 = rfc4627:set_field(J2,"mask",Mask),
			?DEBUG("GROUP ::::> RJ0=~p",[RJ0]),
			J4B = list_to_binary(rfc4627:encode(RJ0)),
			?DEBUG("GROUP ::::> J4B=~p",[J4B]),
			RBody = [{xmlelement,"body",[],[{xmlcdata,J4B}]}],
			RPacket = {X,E,RAttr3,RBody},

			?DEBUG("###### route_group_msg 003 input :::> {From,To,RPacket}=~p",[{From,To,RPacket}]),
			case ejabberd_router:route(From, To, RPacket) of
				ok ->
					?DEBUG("###### route_group_msg 003 OK :::> {From,To,RPacket}=~p",[{From,To,RPacket}]),
					gen_server:cast(aa_hookhandler,{group_chat_filter,From,To,RPacket,false}),
					{ok,ok};
				Err ->
					?DEBUG("###### route_group_msg 003 ERR=~p :::> {From,To,RPacket}=~p",[Err,{From,To,RPacket}]),
					{error,Err}
			end;
		_ ->
			{ok,skip}
	end.

is_group_chat(#jid{server=Domain}=To)->
	DomainTokens = string:tokens(Domain,"."),
	Rtn = case length(DomainTokens) > 2 of 
		true ->
			[G|_] = DomainTokens,
			G=:="group";
		_ ->
			false
	end,
	?DEBUG("##### is_group_chat ::::>To~p ; Rtn=~p",[To,Rtn]),
	Rtn.
