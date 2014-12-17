-module(aa_group_chat).

-include("ejabberd.hrl").
-include("jlib.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([reload_group_user/2]).

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
	gen_server:cast(Pid,{route_group_msg,From,To,Packet}),
	?DEBUG("###### route_group_msg_002 ::::> {From,To,Packet}=~p",[{From,To,Packet}]).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
init([]) ->
	{ok,#state{}}.

handle_cast({route_group_msg,From,To,Packet}, State) ->
	try
		handle_call({route_group_msg,From,To,Packet},[],State) 
	catch 
		_:_ ->
			Err = erlang:get_stacktrace(),
			?ERROR_MSG("route_group_msg_error ~p",[Err])
	end,
	{stop, normal, State};
handle_cast(stop, State) ->
	{stop, normal, State}.

handle_call({route_group_msg,#jid{user=FromUser,server=Domain}=From,#jid{user=GroupId,server=GDomain}=To,Packet}, _From, State) ->
	SYS_Alert = fun() ->
		%%TODO 解散了
		%% <message id="xxxxx" from="yy@group.yuejian.net" to="123456@yuejian.net" type="normal" msgtype=“system”>
		%%	<body>{groupid":"xx","groupname":"",groupmember":[],"type":"15"}</body>
		%% </message>
		{X,E,Attr,_} = Packet,
		RAttr = lists:map(fun({K,V})->
			case K of
				"from" ->
					{K,GroupId++"@"++GDomain};
				"to" ->
					{K,FromUser++"@"++Domain};
				"type" ->
					{K,"normal"};
				"msgtype" ->
					{K,"system"};	
				_ ->
					{K,V}	
			end
		end,Attr),
		{ok,J0,_} = rfc4627:decode("{}"),
		J1 = rfc4627:set_field(J0,"groupid",list_to_binary(GroupId)),
		J2 = rfc4627:set_field(J1,"groupname",<<"">>),
		J3 = rfc4627:set_field(J2,"groupmember",[]),
		J4 = rfc4627:set_field(J3,"type",<<"15">>),
		Json = rfc4627:encode(J4),
		Body = [{xmlelement,"body",[],[{xmlcdata,Json}]}],
		RPacket = {X,E,RAttr,Body},
		gen_server:cast(aa_hookhandler,{group_chat_filter,From,To,RPacket,false}),
		case ejabberd_router:route(To,From,RPacket) of
			ok ->
				?DEBUG("###### route_group_type15 OK :::> {From,To,RPacket}=~p",[{To,From,RPacket}]),
				{ok,ok};
			Err ->
				?DEBUG("###### route_group_type15 ERR=~p :::> {From,To,RPacket}=~p",[Err,{To,From,RPacket}]),
				{error,Err}
		end
	end,
	case get_user_list_by_group_id(Domain,GroupId) of 
		{not_found,_,_,_,_} ->
			SYS_Alert();
		{ok,UserList,Groupmember,Groupname,Masklist} ->
			case UserList of 
				[] ->
					SYS_Alert();
				_ ->
					[JSON] = aa_log:get_text_message_from_packet(Packet),	
					?DEBUG("ROUTE_GROUP ::::> JSON=~p",[JSON]),
					{ok,JO,_} = rfc4627:decode(erlang:list_to_binary(JSON)),
					?DEBUG("ROUTE_GROUP ::::> JO=~p",[JO]),
					RUserList = case rfc4627:get_field(JO,"type") of 
						{ok,<<"18">>} ->
							%% 2014-7-8 : type=18的  system 系统消息，你这边需要特殊处理一下 ......
							case lists:member(list_to_binary(FromUser),UserList) of
								false ->
									lists:append(UserList,[list_to_binary(FromUser)]);
								_ ->
									UserList
							end;
						_ ->
							UserList		
					end,
					case lists:member(list_to_binary(FromUser),RUserList) of
						false ->
							%%TODO 被T了
							%% <message id="xxxxx" from="1@yuejian.net" to"yy@yuejian.net" type="normal" msgtype=“system”>
							%% 	     <body>{groupid":"xx","groupname":"...","groupmember":[...],"type":"14"}</body>
 							%% </message>
							{X,E,Attr,_} = Packet,
							RAttr = lists:map(fun({K,V})->
								case K of
									"from" ->
										{K,GroupId++"@"++GDomain};
									"to" ->
										{K,FromUser++"@"++Domain};
									"type" ->
										{K,"normal"};
									"msgtype" ->
										{K,"system"};
									_ ->
										{K,V}	
								end
							end,Attr),
							{ok,J0,_} = rfc4627:decode("{}"),
							J1 = rfc4627:set_field(J0,"groupid",list_to_binary(GroupId)),
							J2 = rfc4627:set_field(J1,"groupname",Groupname),
							J3 = rfc4627:set_field(J2,"groupmember",Groupmember),
							J4 = rfc4627:set_field(J3,"type",<<"14">>),
							Json = rfc4627:encode(J4),
							Body = [{xmlelement,"body",[],[{xmlcdata,Json}]}],
							RPacket = {X,E,RAttr,Body},
							case ejabberd_router:route(To,From,RPacket) of
								ok ->
									?DEBUG("route_group_type14 OK :::> {From,To,RPacket}=~p",[{To,From,RPacket}]),
									gen_server:cast(aa_hookhandler,{group_chat_filter,From,To,RPacket,false});
								Err ->
									?DEBUG("route_group_type14 ERR=~p :::> {From,To,RPacket}=~p",[Err,{To,From,RPacket}]) 
							end;
						true ->	
							%% -record(jid, {user, server, resource, luser, lserver, lresource}).
							Roster = lists:map(fun(User)-> 
								UID = binary_to_list(User),
								#jid{user=UID,server=Domain,luser=UID,lserver=Domain,resource=[],lresource=[]} 
							end,UserList),
							?DEBUG("###### route_group_msg 002 :::> GroupId=~p ; Roster=~p",[GroupId,Roster]),
							lists:foreach(fun(Target)-> route_msg(From,Target,Packet,GroupId,Groupmember,Groupname,Masklist) end,Roster) 
					end
			end;
		Err ->
			?ERROR_MSG("group_msg_error ~p",Err),
			error
	end,	
	{reply,[],State}.


handle_info({cmd,_Args},State)->
	{noreply,State}.

terminate(_Reason,_State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

reload_group_user(Domain,GroupId) ->
	Response = get_user_list_by_group_id(do,Domain,GroupId),
	Group_cache_key = GroupId++"@"++Domain++"/group_cache",
	case Response of 
		{ok,[],[],[],[]} ->
			skip;
		{not_found,[],[],[],[]} ->
			?DEBUG("reload_group_user__clean__gid=",[GroupId]),
			%% gen_server:call(aa_hookhandler,{ecache_cmd,["DEL",Group_cache_key]});
			%% 20141115: 防止在一个进程排队，产生瓶颈，特此修正
			aa_hookhandler:ecache_cmd(["DEL",Group_cache_key]);
		{ok,_,_,_,_} ->
			?DEBUG("reload_group_user__set__gid=",[GroupId]),
			%% gen_server:call(aa_hookhandler,{ecache_cmd,["SET",Group_cache_key,erlang:term_to_binary(Response)]});
			%% 20141115: 防止在一个进程排队，产生瓶颈，特此修正
			aa_hookhandler:ecache_cmd(["SET",Group_cache_key,erlang:term_to_binary(Response)]);
		_ ->
			skip
	end,
	Response.

get_user_list_by_group_id(Domain,GroupId)->
	case GroupId of 
		"cctest" ->
			{ok,[<<"cc1">>,<<"cc2">>,<<"cc3">>],[],[],[]};
		_ ->
 			case ejabberd_config:get_local_option({group_cache_enable,Domain}) of 
				true ->
					get_user_list_by_group_id(cache,Domain,GroupId);
				_ ->
					get_user_list_by_group_id(do,Domain,GroupId)
			end
	end.
get_user_list_by_group_id(cache,Domain,GroupId) ->
	Group_cache_key = GroupId++"@"++Domain++"/group_cache",
	%% case gen_server:call(aa_hookhandler,{ecache_cmd,["GET",Group_cache_key]}) of
	%% 20141115: 防止在一个进程排队，产生瓶颈，特此修正
	case aa_hookhandler:ecache_cmd(["GET",Group_cache_key]) of
		{ok,Bin} when erlang:is_binary(Bin) ->
			erlang:binary_to_term(Bin);
		_ ->
			reload_group_user(Domain,GroupId)
	end;
get_user_list_by_group_id(do,Domain,GroupId) when is_binary(Domain) ->
	get_user_list_by_group_id(do,binary_to_list(Domain),GroupId);
get_user_list_by_group_id(do,Domain,GroupId) when is_list(Domain) ->
	?DEBUG("###### get_user_list_by_group_id :::> Domain=~p ; GroupId=~p",[Domain,GroupId]),
	GroupId_bin = case is_binary(GroupId) of 
		true -> 
			GroupId ; 
		_->
			list_to_binary(GroupId)
	end,
 	HTTPTarget =  ejabberd_config:get_local_option({http_server,Domain}),
	{M,S,SS} = now(),
	SN_T = erlang:integer_to_list(M*1000000000000+S*1000000+SS),
	{Service,Method,GID,SN} = {
			list_to_binary("ejabberd"),
			list_to_binary("getUserList"),
			GroupId_bin,
			list_to_binary(SN_T)
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
								case rfc4627:get_field(Obj,"entity") of
									{ok,Entity} ->
										?DEBUG("###### success=true get_user_list_by_group_id :::> entity=~p",[Entity]),
										try
											{ok,UserList} = rfc4627:get_field(Entity,"userlist"),
											{ok,Groupmember} = rfc4627:get_field(Entity,"groupmember"),
											{ok,Masklist} = rfc4627:get_field(Entity,"masklist"),
											{ok,Groupname} = rfc4627:get_field(Entity,"groupname"),
											{ok,UserList,Groupmember,Groupname,Masklist}
										catch
											_:_->
												{not_found,[],[],[],[]}
										end;
									_ ->
										{ok,[],[],[],[]}
								end;
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
 				?ERROR_MSG("[aa_group_chat__http_callback__ERROR] cause ~p~n",[Reason]),
				{error,Reason}
     	end.

route_msg(#jid{user=FromUser}=From,#jid{user=User,server=Domain}=To,Packet,GroupId,Groupmember,Groupname,Masklist) ->
	case FromUser=/=User of
		true->
			{X,E,Attr,Body} = Packet,
			?DEBUG("##### route_group_msg_003 param :::> {User,Domain,GroupId,Masklist}=~p",[{User,Domain,GroupId,Masklist}]),
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
			Mask = case lists:member(list_to_binary(User),Masklist) of true -> "1"; false-> "0" end,
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
			RJ0 = rfc4627:set_field(J2,"mask",list_to_binary(Mask)),
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
