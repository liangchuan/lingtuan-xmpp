-module(aa_process_counter).

-export([process_counter/0,msg_counter/3]).

-include("ejabberd.hrl").

process_counter()->
	Counter = lists:map(fun process_counter/1,[node()|nodes()]),
	L = [X||X<-Counter,ordsets:is_subset([ok],tuple_to_list(X))=:=true],
	process_counter_to_json(L,[]).
process_counter(From) ->
	try
		P = rpc:call(From,erlang,whereis,[ejabberd_c2s_sup]),
		case is_pid(P) of 
			true ->
				D = dict:from_list( rpc:call(From,erlang,process_info,[P]) ),
				L = dict:fetch('links',D),
				{ok,{From,length(L)}};
			_ ->
				{no_ejabberd_node,From}
		end
	catch
		_:_->
			{no_ejabberd_node,From}
	end.
process_counter_to_json([E|L],List)->
	{ok,{Node,Total}} = E,
	JN = {obj,[{node,Node},{totalCount,Total}]},	
	process_counter_to_json(L,[JN|List]);
process_counter_to_json([],List)->
	List.



%%%ÏûÏ¢¼ÆËã
msg_counter(Y,M,D) when erlang:is_binary(Y)->
	DataValue = binary_to_list(Y)++binary_to_list(M)++binary_to_list(D),
	KEY = "calculat_msg_"++DataValue,
	case aa_hookhandler:ecache_cmd(["GET",KEY]) of
		{ok,undefined}->
			0;
		{ok,R}->
			erlang:binary_to_integer(R);
		_->
			0
	end;
msg_counter(Y,M,D)->
	DataValue = integer_to_list(Y)++integer_to_list(M)++integer_to_list(D),
	KEY = "calculat_msg_"++DataValue,
	case aa_hookhandler:ecache_cmd(["GET",KEY]) of
		{ok,undefined}->
			0;
		{ok,R}->
			erlang:binary_to_integer(R);
		_->
			0
	end.