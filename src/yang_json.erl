-module(yang_json).
-compile(export_all).

json_rpc(YangFile) ->
    Dir = filename:dirname(YangFile),
    case yang:parse_file(YangFile) of
	{ok, Y} ->
	    each_module(fun to_json_rpc/2, Y, Dir);
	Error ->
	    Error
    end.

each_module(F, [{module,_,M,Data}|Rest], Dir) ->
    [{module, binary_to_list(M), F(Data, Dir)} | each_module(F, Rest, Dir)];
each_module(F, [_|Rest], Dir) ->
    each_module(F, Rest, Dir);
each_module(_, [], _) ->
    [].

to_json_rpc(Data, Dir) ->
    Imports = imports(Data, Dir),
    [{binary_to_list(N), mk_rpc_pair(InOut, N, Data, Imports)} || {rpc,_,N,InOut} <- Data].

imports(Data, Dir) ->
    lists:foldl(
      fun({import,_,F,[{prefix,_,Pfx,_}|_]}, Acc) ->
	      case yang:parse_file(filename:join(Dir, binary_to_list(F) ++ ".yang")) of
		  {ok, Y} ->
		      case [D || {module,_,N,D} <- Y,
				 N == F] of
			  [ImpData] ->
			      orddict:store(Pfx, ImpData, Acc);
			  [] ->
			      %% error({cannot_import, F, no_such_module})
			      Acc
		      end;
		  Error ->
		      %% error({cannot_import, F, Error})
		      Acc
	      end;
	 (_, Acc) ->
	      Acc
      end, orddict:new(), Data).

mk_rpc_pair(InOut, N, Data, Imports) ->
    {_,_,_,I} = lists:keyfind(input, 1, InOut),
    {_,_,_,O} = lists:keyfind(output, 1, InOut),
    {{request, {struct, [{"json-rpc", "2.0"},
			 {"method", binary_to_list(N)},
			 {"id", ""},
			 {"params", {struct, rpc_params(I, Data, Imports)}}]}},
     {reply, {struct, [{"json-rpc", "2.0"},
		       {"id", ""},
		       {"result", {struct, rpc_params(O, Data, Imports)}}]}}}.

rpc_params([{uses,_,G,_}|T], Data, Imports) ->
    {Where, GrpName} = case re:split(G, <<":">>) of
			   [_] ->
			       {Data, G};
			   [Pfx, G1] ->
			       {orddict:fetch(Pfx, Imports), G1}
		       end,
    case [L || {grouping,_,Grp,L} <- Where,
	       Grp == GrpName] of
	[] ->
	    [{"uses-not-found", binary_to_list(G)} | rpc_params(T, Data, Imports)];
	[Params] ->
	    rpc_params(Params, Data, Imports) ++ rpc_params(T, Data, Imports)
    end;
rpc_params([{leaf,_,N,_}|T], Data, Imports) ->
    [{binary_to_list(N), ""} | rpc_params(T, Data, Imports)];
rpc_params([{anyxml,_,N,_}|T], Data, Imports) ->
    [{binary_to_list(N), ""} | rpc_params(T, Data, Imports)];
rpc_params([{list,_,N,Items}|T], Data, Is) ->
    [{binary_to_list(N), {array, [{struct, rpc_params(Items, Data, Is)}]}} | rpc_params(T, Data, Is)];
rpc_params([_|T], Data, Is) ->
    rpc_params(T, Data, Is);
rpc_params([], _, _) ->
    [].

markdown(File, JSON) ->
    case file:open(File, [write]) of
	{ok, Fd} ->
	    try io:fwrite(Fd, "~s~n", [markdown(JSON)])
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

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
    ["{", pp_json(H, I1), [[",\n", i(I1), pp_json(Term,I1)] || Term <- T], "}"];
pp_json({array, []}, _I) ->
    ["[]"];
pp_json({array, [H|T]}, I) ->
    I1 = I+1,
    ["[", pp_json(H, I1), [[",\n", i(I1), pp_json(Term,I1)] || Term <- T], "]\n"];
pp_json(V, _I) when is_binary(V); is_list(V) ->
    io_lib:fwrite("\"~s\"", [V]);
pp_json(V, _I) when is_integer(V) ->
    ["\"", integer_to_list(V), "\""];
pp_json({K,V}, I) ->
    Part1 = ["\"", K, "\": "],
    I1 = I + iolist_size(Part1),
    [Part1, pp_json(V, I1)].

i(I) ->
    lists:duplicate(I,$\s).
