-module(yang_json).
-compile(export_all).

json_rpc(YangFile) ->
    case yang:parse_file(YangFile) of
	{ok, Y} ->
	    each_module(fun to_json_rpc/1, Y);
	Error ->
	    Error
    end.

each_module(F, [{module,_,M,Data}|Rest]) ->
    [{module, binary_to_list(M), F(Data)} | each_module(F, Rest)];
each_module(F, [_|Rest]) ->
    each_module(F, Rest);
each_module(_, []) ->
    [].

to_json_rpc(Data) ->
    [{binary_to_list(N), mk_rpc_pair(InOut, N, Data)} || {rpc,_,N,InOut} <- Data].

mk_rpc_pair(InOut, N, Data) ->
    {_,_,_,I} = lists:keyfind(input, 1, InOut),
    {_,_,_,O} = lists:keyfind(output, 1, InOut),
    {{request, {struct, [{"json-rpc", "2.0"},
			 {"method", binary_to_list(N)},
			 {"id", ""},
			 {"params", {struct, rpc_params(I, Data)}}]}},
     {reply, {struct, [{"json-rpc", "2.0"},
		       {"id", ""},
		       {"result", {struct, rpc_params(O, Data)}}]}}}.

rpc_params([{uses,_,G,_}|T], Data) ->
    case [L || {grouping,_,Grp,L} <- Data,
	       Grp == G] of
	[] ->
	    [{"uses-not-found", binary_to_list(G)} | rpc_params(T, Data)];
	[Params] ->
	    rpc_params(Params, Data) ++ rpc_params(T, Data)
    end;
rpc_params([{leaf,_,N,_}|T], Data) ->
    [{binary_to_list(N), ""} | rpc_params(T, Data)];
rpc_params([{anyxml,_,N,_}|T], Data) ->
    [{binary_to_list(N), ""} | rpc_params(T, Data)];
rpc_params([{list,_,N,Items}|T], Data) ->
    [{binary_to_list(N), {struct, rpc_params(Items, Data)}} | rpc_params(T, Data)];
rpc_params([_|T], Data) ->
    rpc_params(T, Data);
rpc_params([], _) ->
    [].


markdown([{module, M, RPCs}|T]) ->
    ["## Module: ", M, "\n\n", markdown_rpcs(RPCs), "\n\n" | markdown(T)];
markdown([]) ->
    [].

markdown_rpcs([{RPC, {{request, Req}, {reply, Rep}}} | T]) ->
    ["### RPC: ", RPC, "\n\n",
     "#### Request\n", "```json\n", pp_json(Req), "\n```\n",
     "#### Reply\n", "```json\n", pp_json(Rep), "\n```\n\n" |
     markdown_rpcs(T)];
markdown_rpcs([]) ->
    [].

pp_json(Json) ->
    pp_json(Json, 0).

pp_json({struct, []}, I) ->
    [i(I), "{}"];
pp_json({struct, [H|T]}, I) ->
    I1 = I+1,
    ["{", pp_term(H, I1), [[",\n", i(I1), pp_term(Term,I1)] || Term <- T], "}"];
pp_json({array, []}, _I) ->
    ["[]"];
pp_json({array, [H|T]}, I) ->
    I1 = I+1,
    ["[", pp_term(H, I1), [[",\n", i(I1), pp_term(Term,I1)] || Term <- T], "]\n"];
pp_json(V, _I) when is_binary(V); is_list(V) ->
    io_lib:fwrite("\"~s\"", [V]);
pp_json(V, _I) when is_integer(V) ->
    ["\"", integer_to_list(V), "\""].

pp_term({K, V}, I) ->
    Part1 = ["\"", K, "\": "],
    I1 = I + iolist_size(Part1),
    [Part1, pp_json(V, I1)].

i(I) ->
    lists:duplicate(I,$\s).
