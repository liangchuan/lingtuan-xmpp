%%%----------------------------------------------------------------------
%%% File    : ejabberd_auth_external.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Authentification via LDAP external script
%%% Created : 12 Dec 2004 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2013   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(ejabberd_auth_external).
-author('zxflimit@gmail.com').

%% External exports
-export([start/1,
	 set_password/3,
	 check_password/3,
	 check_password/5,
	 try_register/3,
	 dirty_get_registered_users/0,
	 get_vh_registered_users/1,
	 get_vh_registered_users/2,
	 get_vh_registered_users_number/1,
	 get_vh_registered_users_number/2,
	 get_password/2,
	 get_password_s/2,
	 is_user_exists/2,
	 remove_user/2,
	 remove_user/3,
	 store_type/0,
	 plain_password_required/0
	]).

-include("ejabberd.hrl").

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host) ->
	inets:start().

check_cache_last_options(Server) ->
    %% if extauth_cache is enabled, then a mod_last module must also be enabled
    case get_cache_option(Server) of
	false -> no_cache;
	{true, _CacheTime} ->
	    case get_mod_last_configured(Server) of
		no_mod_last ->
		    ?ERROR_MSG("In host ~p extauth is used, extauth_cache is enabled but "
			       "mod_last is not enabled.", [Server]),
		    no_cache;
		_ -> cache
	    end
    end.

plain_password_required() ->
    true.

store_type() ->
	external.

check_password(User, Server, Password) ->
    case get_cache_option(Server) of
	false -> check_password_extauth(User, Server, Password);
	{true, CacheTime} -> check_password_cache(User, Server, Password, CacheTime)
    end.

check_password(User, Server, Password, _Digest, _DigestGen) ->
    check_password(User, Server, Password).

set_password(User, Server, Password) ->
    case extauth:set_password(User, Server, Password) of
	true -> set_password_internal(User, Server, Password),
		ok;
	_ -> {error, unknown_problem}
    end.

try_register(User, Server, Password) ->
    case get_cache_option(Server) of
	false -> try_register_extauth(User, Server, Password);
	{true, _CacheTime} -> try_register_external_cache(User, Server, Password)
    end.

dirty_get_registered_users() ->
    ejabberd_auth_internal:dirty_get_registered_users().

get_vh_registered_users(Server) ->
    ejabberd_auth_internal:get_vh_registered_users(Server).

get_vh_registered_users(Server, Data)  ->
    ejabberd_auth_internal:get_vh_registered_users(Server, Data).

get_vh_registered_users_number(Server) ->
    ejabberd_auth_internal:get_vh_registered_users_number(Server).

get_vh_registered_users_number(Server, Data) ->
    ejabberd_auth_internal:get_vh_registered_users_number(Server, Data).

%% The password can only be returned if cache is enabled, cached info exists and is fresh enough.
get_password(User, Server) ->
    case get_cache_option(Server) of
	false -> false;
	{true, CacheTime} -> get_password_cache(User, Server, CacheTime)
    end.

get_password_s(User, Server) ->
    case get_password(User, Server) of
	false -> [];
	Other -> Other
    end.

%% @spec (User, Server) -> true | false | {error, Error}
is_user_exists(User, Server) ->
	?INFO_MSG("########~p AUTH__Server=~p~n",[is_user_exists,Server]),
	HTTPServer =  ejabberd_config:get_local_option({http_server,Server}),
	HTTPService = ejabberd_config:get_local_option({http_server_service_client,Server}),
	HTTPTarget = string:concat(HTTPServer,HTTPService),
	HTTPHead = "application/x-www-form-urlencoded",
	%% 2013-10-22 : 新的请求协议如下，此处不必关心，success=true 即成功
	%% INPUT {"username":"xxx"}
	%% OUTPUT {"success":true,"entity":true}
	UID = list_to_binary(User),
	{Service,Method,Channel} = {list_to_binary("service.uri.pet_sso"),list_to_binary("isUsernameInuse"),list_to_binary("9")},
	PostBody = {obj,[{"service",Service},{"method",Method},{"channel",Channel},{"params",{obj,[{"username",UID}]}}]},	
	Form = "body="++rfc4627:encode(PostBody),
	?INFO_MSG("##########################~p~n HTTP_TARGET=~p~nHTTP_PARAM=~p~n",[is_user_exists,HTTPTarget,Form]),
	case httpc:request(post,{HTTPTarget,[],HTTPHead, Form},[],[]) of   
        	{ok, {_,_,Body}} -> 
			case rfc4627:decode(Body) of
				{ok,Obj,_} -> 
					case rfc4627:get_field(Obj,"success") of
						{ok,true} ->
							true;
						{ok,false} ->
							{ok,Entity} = rfc4627:get_field(Obj,"entity"),
							?INFO_MSG("liangc-login error: ~p~n",[binary_to_list(Entity)]),
							false;
						_ ->
							false
					end;
				_ -> 
					false
			end ;
        	{error, Reason} ->
			?INFO_MSG("error cause ~p~n",[Reason])  
    	end.

remove_user(User, Server) ->
    case extauth:remove_user(User, Server) of
	false -> false;
	true ->
	    case get_cache_option(Server) of
		false -> false;
		{true, _CacheTime} ->
		    ejabberd_auth_internal:remove_user(User, Server)
	    end
    end.

remove_user(User, Server, Password) ->
    case extauth:remove_user(User, Server, Password) of
	false -> false;
	true ->
	    case get_cache_option(Server) of
		false -> false;
		{true, _CacheTime} ->
		    ejabberd_auth_internal:remove_user(User, Server, Password)
	    end
    end.

%%%
%%% Extauth cache management
%%%

%% @spec (Host::string()) -> false | {true, CacheTime::integer()}
get_cache_option(Host) ->
    case ejabberd_config:get_local_option({extauth_cache, Host}) of
	CacheTime when is_integer(CacheTime) -> {true, CacheTime};
	_ -> false
    end.

%% @spec (User, Server, Password) -> true | false
check_password_extauth(User, Server, Password) ->
	?INFO_MSG("##########################~p AUTH_Server=~p~nUser=~p~n",[check_password_extauth,Server,User]),
	HTTPServer =  ejabberd_config:get_local_option({http_server,Server}),
	HTTPService = ejabberd_config:get_local_option({http_server_service_client,Server}),
	HTTPTarget = string:concat(HTTPServer,HTTPService),
	HTTPHead = "application/x-www-form-urlencoded",
	%% 2013-10-22 : 新的请求协议如下,其中输出的 entity 属性中带有 token等信息，此处不必关心，success=true 即成功
	%% INPUT {"service":"service.uri.pet_sso","method":"token","channel":"9","token":PWD}
	%% OUTPUT {"success":true,"entity":{}}
	{PWD} = {list_to_binary(Password)},
	{Service,Method,Channel} = {list_to_binary("service.uri.pet_sso"),list_to_binary("token"),list_to_binary("9")},
	PostBody = {obj,[{"service",Service},{"method",Method},{"channel",Channel},{"token",PWD}]},	
	Form = "body="++rfc4627:encode(PostBody),
	?INFO_MSG("##########################~p~nHTTP_TARGET=~p~nFORM=~p~n",[check_password_extauth,HTTPTarget,Form]),
    	case httpc:request(post,{HTTPTarget,[],HTTPHead, Form},[],[]) of   
        	{ok, {_,_,Body}}-> 
			case rfc4627:decode(Body) of 
				{ok,Obj,_Re} -> 
					case rfc4627:get_field(Obj,"success") of 
						{ok,true} -> 
							true;
						{ok,false} ->
							{ok,Entity} = rfc4627:get_field(Obj,"entity"),
							?INFO_MSG("liangc-login error: ~p~n",[binary_to_list(Entity)]),
							false;
						_ -> 
							false
					end;
				{error, Reason}->
					?INFO_MSG("error cause ~p~n",[Reason]) 
			end ;
        	{error, Reason}->
			?INFO_MSG("error cause ~p~n",[Reason])  
    	end.

%% @spec (User, Server, Password) -> true | false
try_register_extauth(User, Server, Password) ->
    extauth:try_register(User, Server, Password).

check_password_cache(User, Server, Password, CacheTime) ->
    case get_last_access(User, Server) of
	online ->
	    check_password_internal(User, Server, Password);
	never ->
	    check_password_external_cache(User, Server, Password);
	mod_last_required ->
	    ?ERROR_MSG("extauth is used, extauth_cache is enabled but mod_last is not enabled in that host", []),
	    check_password_external_cache(User, Server, Password);
	TimeStamp ->
	    %% If last access exists, compare last access with cache refresh time
	    case is_fresh_enough(TimeStamp, CacheTime) of
		%% If no need to refresh, check password against Mnesia
		true ->
		    case check_password_internal(User, Server, Password) of
			%% If password valid in Mnesia, accept it
			true ->
			    true;
			%% Else (password nonvalid in Mnesia), check in extauth and cache result
			false ->
			    check_password_external_cache(User, Server, Password)
		    end;
		%% Else (need to refresh), check in extauth and cache result
		false ->
		    check_password_external_cache(User, Server, Password)
	    end
    end.

get_password_internal(User, Server) ->
    ejabberd_auth_internal:get_password(User, Server).

%% @spec (User, Server, CacheTime) -> false | Password::string()
get_password_cache(User, Server, CacheTime) ->
    case get_last_access(User, Server) of
	online ->
	    get_password_internal(User, Server);
	never ->
	    false;
	mod_last_required ->
	    ?ERROR_MSG("extauth is used, extauth_cache is enabled but mod_last is not enabled in that host", []),
	    false;
	TimeStamp ->
	    case is_fresh_enough(TimeStamp, CacheTime) of
		true ->
		    get_password_internal(User, Server);
		false ->
		    false
	    end
    end.


%% Check the password using extauth; if success then cache it
check_password_external_cache(User, Server, Password) ->
    case check_password_extauth(User, Server, Password) of
	true ->
	    set_password_internal(User, Server, Password), true;
	false ->
	    false
    end.

%% Try to register using extauth; if success then cache it
try_register_external_cache(User, Server, Password) ->
    case try_register_extauth(User, Server, Password) of
	{atomic, ok} = R ->
	    set_password_internal(User, Server, Password),
	    R;
	_ -> {error, not_allowed}
    end.

%% @spec (User, Server, Password) -> true | false
check_password_internal(User, Server, Password) ->
    ejabberd_auth_internal:check_password(User, Server, Password).

%% @spec (User, Server, Password) -> ok | {error, invalid_jid}
set_password_internal(User, Server, Password) ->
    ejabberd_auth_internal:set_password(User, Server, Password).

%% @spec (TimeLast, CacheTime) -> true | false
%%       TimeLast = online | never | integer()
%%       CacheTime = integer() | false
is_fresh_enough(online, _CacheTime) ->
    true;
is_fresh_enough(never, _CacheTime) ->
    false;
is_fresh_enough(TimeStampLast, CacheTime) ->
    {MegaSecs, Secs, _MicroSecs} = now(),
    Now = MegaSecs * 1000000 + Secs,
    (TimeStampLast + CacheTime > Now).

%% @spec (User, Server) -> online | never | mod_last_required | TimeStamp::integer()
%% Code copied from mod_configure.erl
%% Code copied from web/ejabberd_web_admin.erl
%% TODO: Update time format to XEP-0202: Entity Time
get_last_access(User, Server) ->
    case ejabberd_sm:get_user_resources(User, Server) of
	[] ->
	    _US = {User, Server},
	    case get_last_info(User, Server) of
		mod_last_required ->
		    mod_last_required;
		not_found ->
		    never;
		{ok, Timestamp, _Status} ->
		    Timestamp
	    end;
	_ ->
	    online
    end.
%% @spec (User, Server) -> {ok, Timestamp, Status} | not_found | mod_last_required
get_last_info(User, Server) ->
    case get_mod_last_enabled(Server) of
	mod_last -> mod_last:get_last_info(User, Server);
	no_mod_last -> mod_last_required
    end.

%% @spec (Server) -> mod_last | no_mod_last
get_mod_last_enabled(Server) ->
    case gen_mod:is_loaded(Server, mod_last) of
        true -> mod_last;
	false -> no_mod_last
    end.

get_mod_last_configured(Server) ->
    case is_configured(Server, mod_last) of
        true -> mod_last;
	false -> no_mod_last
    end.

is_configured(Host, Module) ->
    lists:keymember(Module, 1, ejabberd_config:get_local_option({modules, Host})).
