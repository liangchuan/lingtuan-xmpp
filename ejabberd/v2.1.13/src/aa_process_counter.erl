-module(aa_process_counter).

-export([process_counter/0]).

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
