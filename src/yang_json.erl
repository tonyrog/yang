%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2007 - 2012, Rogvall Invest AB, <tony@rogvall.se>
%%%
%%% This software is licensed as described in the file COPYRIGHT, which
%%% you should have received as part of this distribution. The terms
%%% are also available at http://www.rogvall.se/docs/copyright.txt.
%%%
%%% You may opt to use, copy, modify, merge, publish, distribute and/or sell
%%% copies of the Software, and permit persons to whom the Software is
%%% furnished to do so, under the terms of the COPYRIGHT file.
%%%
%%% This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
%%% KIND, either express or implied.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
-module(yang_json).
-compile(export_all).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(fd(Fl, A, B), Fl when A =< X, X =< B).

to_json_type(X, {type,_,<<"decimal64">>,I} = T) when is_float(X) ->
    F = case lists:keyfind('fraction-digits', 1, I) of
	    false -> 18;
	    {_,_,F1,_} -> list_to_integer(binary_to_list(F1))
	end,
    case F of
	?fd(1,-922337203685477580.8, 922337203685477580.7) -> ok;
	?fd(2, -92233720368547758.08, 92233720368547758.07) -> ok;
	?fd(3, -9223372036854775.808, 9223372036854775.807) -> ok;
	?fd(4, -922337203685477.5808, 922337203685477.5807) -> ok;
	?fd(5, -92233720368547.75808, 92233720368547.75807) -> ok;
	?fd(6, -9223372036854.775808, 9223372036854.775807) -> ok;
	?fd(7, -922337203685.4775808, 922337203685.4775807) -> ok;
	?fd(8, -92233720368.54775808, 92233720368.54775807) -> ok;
	?fd(9, -9223372036.854775808, 9223372036.854775807) -> ok;
	?fd(10, -922337203.6854775808, 922337203.6854775807) -> ok;
	?fd(11, -92233720.36854775808, 92233720.36854775807) -> ok;
	?fd(12, -9223372.036854775808, 9223372.036854775807) -> ok;
	?fd(13, -922337.2036854775808, 922337.2036854775807) -> ok;
	?fd(14, -92233.72036854775808, 92233.72036854775807) -> ok;
	?fd(15, -9223.372036854775808, 9223.372036854775807) -> ok;
	?fd(16, -922.3372036854775808, 922.3372036854775807) -> ok;
	?fd(17, -92.23372036854775808, 92.23372036854775807) -> ok;
	?fd(18, -9.223372036854775808, 9.223372036854775807) -> ok;
	_ -> error({type_error, [X, T]})
    end,
    list_to_binary(io_lib:fwrite("~." ++ integer_to_list(F) ++ "f", [X]));
to_json_type(X, {type,_,<<"int", _/binary>>,_}) when is_integer(X) ->
    %% list_to_binary(integer_to_list(X));
    X;
to_json_type(X, {type,_,<<"string">>,_}) when is_atom(X) ->
    atom_to_binary(X, latin1);
to_json_type(X, {type,_,<<"string">>,_}) when is_list(X) ->
    iolist_to_binary(X);
to_json_type(X, {type,_,<<"string">>,_}) when is_integer(X) ->
    list_to_binary(integer_to_list(X));
to_json_type(_, void) -> <<"ok">>;
to_json_type(X, {type,_,<<"uint", _/binary>>,_}) when is_integer(X), X >= 0 ->
    X;
to_json_type(false, {type,_,<<"boolean">>,_}) -> false;
to_json_type(true, {type,_,<<"boolean">>,_}) -> true;
to_json_type(X0, {type,_,<<"enumeration">>, En} = T) ->
    %% We always return the key (binary) - not the value (integer).
    %% This seems to be in keeping with how NETCONF does it.
    X = if is_integer(X0) -> list_to_binary(integer_to_list(X0));
           is_binary(X0)  -> X0;
           is_list(X0) -> list_to_binary(X0)
        end,
    case find_enum(En, X) of
        [] -> error({type_error, [X0, T]});
        [{Key, _}] ->
            Key;
        [{K1, V1}, {K2, V2}] ->
            %% Either V1 or V2 must be equal to X, or it's an error
            if V1 == X -> K1;
               V2 == X -> K2;
               true -> error({type_error, [X0, T]})
            end
    end;
to_json_type(X, {type, _, <<"union">>, Ts} = Type) ->
    to_json_type_union(Ts, X, Type);
to_json_type(_, {type, _, <<"empty">>, _}) -> <<>>;
to_json_type(X, _) ->
    X.

to_json_type_union([T|Ts], X, Type) ->
    try to_json_type(X, T)
    catch
	error:_ ->
	    to_json_type_union(Ts, X, Type)
    end;
to_json_type_union([], X, Type) ->
    error({type_error, [X, Type]}).

json_rpc(YangFile) ->
    json_rpc(YangFile, []).

json_rpc(YangFile, Opts0) ->
    Dir = filename:dirname(YangFile),
    Opts = add_path(Dir, Opts0),
    case read(YangFile, Ext = filename:extension(YangFile), Opts) of
	{{ok, Y}, Ext} ->
	    with_module(fun to_json_rpc/3, Y, {Dir, Ext, Opts});
	{Error, _} ->
	    Error
    end.

add_path(".", Opts) ->
    Opts;
add_path(D, Opts) ->
    Opts ++ [{path, D}].


hrl(YangFile, HrlFile) ->
    hrl(YangFile, HrlFile, []).

hrl(YangFile, HrlFile, Opts) ->
    [{module, M, RPCs}] = json_rpc(YangFile, Opts),
    Forms = to_hrl_forms(RPCs, M),
    write_hrl(HrlFile, Forms, Opts).

write_hrl(_HrlFile, _Forms, _Opts) ->
    error(nyi).

read(F) ->
    read(F, filename:extension(F), []).

read(F, ".yang", Opts) ->
    {yang_parser:deep_parse(F, Opts), ".yang"};
read(F, ".eterm", Opts) ->
    {read_eterm(F, Opts), ".eterm"}.

read_eterm(F, Opts) ->
    case lists:keyfind(open_hook, 1, Opts) of
	{_, Open} ->
	    case Open(F, Opts) of
		{ok, Fd} ->
		    try
			Bin = read_open_file(Fd),
			{ok, binary_to_term(Bin)}
		    after
			file:close(Fd)
		    end;
		{error, _} = E ->
		    E
	    end;
	false ->
	    case file:read_file(F) of
		{ok, Bin} ->
		    {ok, binary_to_term(Bin)};
		Error ->
		    Error
	    end
    end.

read_open_file(Fd) ->
    file:position(Fd, bof),
    Read = fun() -> file:read(Fd, 4096) end,
    read_open_file(Read(), Read, <<>>).

read_open_file(eof, _, Acc) ->
    Acc;
read_open_file({ok, Bytes}, Read, Acc) ->
    read_open_file(Read(), Read, <<Acc/binary, Bytes/binary>>).


with_module(F, [{module,_,M,Data}], Dir) ->
    Ms = binary_to_list(M),
    [{module, Ms, F(Data, Dir, Ms)}].


to_hrl_forms(RPCs, _M) ->
    Macros = macro_forms(RPCs, _M),
    Records = record_defs(RPCs, _M),
    Macros ++ Records.

macro_forms(_RPCs, _M) ->
    [].

record_defs(_RPCs, _M) ->
    [].

remove_yang_info([{K, [], _}|T]) ->
    [{K, []} | remove_yang_info(T)];
remove_yang_info([{K, [H1|_] = Ch, _}|T]) when is_tuple(H1) ->
    [{K, remove_yang_info(Ch)} | remove_yang_info(T)];
remove_yang_info([{K, V, _}|T]) ->
    [{K, V} | remove_yang_info(T)];
remove_yang_info([{_,_} = H|T]) ->
    [H | remove_yang_info(T)];
remove_yang_info([]) ->
    [].

validate_request({call, Method, {struct, Request}}, [{module,_,M,Elems}]) ->
    ShortMethod = case re:split(Method, ":", [{return, binary}]) of
                      [M, Meth] ->
                          Meth;
                      [Meth]    -> Meth;
                      [_OtherM, _] ->
                          error({unknown_method, Method})
                  end,
    %% we have the right module
    case find_rpc(Elems, ShortMethod) of
        error ->
            {error, {unknown_method, Method}};
        {rpc, _, _ActualMethod, Data} ->
            {input,_,_,InputElems} = lists:keyfind(input, 1, Data),
            validate_rpc_request(InputElems, Request)
    end.

find_rpc([{rpc, _, Meth, _} = RPC|_], Meth) ->
    RPC;
find_rpc([_|Elems], Meth) ->
    find_rpc(Elems, Meth);
find_rpc([], _) ->
    error.

validate_rpc_request(Req, Data) ->
    try begin
            {ValidReq, Meta} = validate_rpc_request(Req, Data, [], []),
            {ok, ValidReq, Meta}
        end
    catch
        throw:Error ->
            {error, Error}
    end.

validate_rpc_request([{Leaf,_,Key,Info}|Elems], Data, Acc, Meta)
  when Leaf == leaf; Leaf == anyxml ->
    case keytake(Key, Data) of
        {{_, Value}, Data1} ->
            Value1 = convert_elem(Leaf, Key, Value, Info),
            validate_rpc_request(
              Elems, Data1,
              [{to_atom(Key), Value1, Info}|Acc], Meta);
        false ->
            validate_rpc_request_missing(Leaf, Elems, Key, Info, Data, Acc, Meta)
    end;
validate_rpc_request([{Stmt, _, Key, Info}|Elems], Data, Acc, Meta)
  when Stmt==list; Stmt=='leaf-list' ->
    case keytake(Key, Data) of
        false ->
            validate_rpc_request_missing(Stmt, Elems, Key, Info, Data, Acc, Meta);
        {Found, Data1} ->
            validate_rpc_request(
              Elems, Data1,
              [validate_rpc_elem(Stmt, Found, Info)|Acc], Meta)
    end;
validate_rpc_request([Other|Elems], Data, Acc, Meta) ->
    validate_rpc_request(Elems, Data, Acc, [Other|Meta]);
validate_rpc_request([], [], Acc, Meta) ->
    {lists:reverse(Acc), lists:reverse(Meta)};
validate_rpc_request([], [_|_] = Unknown, _, _) ->
    throw({invalid_params, {unknown_params, [element(1, U) || U <- Unknown]}}).

validate_rpc_elem(list, {Key, {array, ListElems}}, Info) ->
    case lists:map(fun({struct, StructElems}) ->
                           validate_rpc_request(Info, StructElems, [], [])
                   end, ListElems) of
        [] ->
            check_min_elements([], Key, Info),
            %% Meta = [I || I <- Info,
            %%           not lists:member(element(1,I),
            %%                            [leaf, anyxml, list, 'leaf-list'])],
            {to_atom(Key), [], Info};
        [_|_] = List ->
            Len = length(List),
            check_min_elements_(Len, Key, lists:keyfind('min-elements',1,Info)),
            check_max_elements_(Len, Key, lists:keyfind('max-elements',1,Info)),
            {to_atom(Key), [[{X,V} || {X,V,_} <- L]
                                           || {L,_} <- List], Info}
    end;
validate_rpc_elem('leaf-list', {Key, {array, ListElems}}, Info) ->
    {type,_,_,_} = Type = lists:keyfind(type, 1, Info),
    List =  [convert_elem(leaf, Key, Value, Type, Info) || Value <- ListElems],
    Length = length(List),
    check_min_elements_(Length, Key, lists:keyfind('min-elements',1,Info)),
    check_max_elements_(Length, Key, lists:keyfind('max-elements',1,Info)),
    {to_atom(Key), List, Info};
validate_rpc_elem(Stmt, {Key, Other}, _Info)
  when Stmt==list; Stmt=='leaf-list' ->
    throw({invalid_params, {mismatch, [Key, Other, array]}}).

check_max_elements(List, Key, Info) ->
    case lists:keyfind('max-elements', 1 , Info) of
        {_, _, _, _} = Max ->
            Length = length(List),
            check_max_elements_(Length, Key, Max);
        false ->
            ok
    end.

check_max_elements_(_, _Key, {'max-elements',_,<<"unbounded">>,_}) ->
    ok;
check_max_elements_(Length, Key, {'max-elements',_,Max,_}) ->
    case list_to_integer(binary_to_list(Max)) of
        N when N < Length ->
            throw({invalid_params, {'max-elements', [Key, N, Length]}});
        _ ->
            ok
    end;
check_max_elements_(_, _, false) ->
    ok.

check_min_elements(List, Key, Info) ->
    case lists:keyfind('min-elements', 1 , Info) of
        {_, _, _, _} = Max ->
            Length = length(List),
            check_min_elements_(Length, Key, Max);
        false ->
            ok
    end.

check_min_elements_(Length, Key, {'min-elements',_,Max,_}) ->
    case list_to_integer(binary_to_list(Max)) of
        N when N > Length ->
            throw({invalid_params, {'min-elements', [Key, N, Length]}});
        _ ->
            ok
    end;
check_min_elements_(_, _, false) ->
    ok.

validate_rpc_request_missing(Stmt, Elems, Key, Info, Data, Acc, Meta)
  when Stmt==list; Stmt=='leaf-list' ->
    case lists:keyfind('min-elements', 1, Info) of
        {_, _, <<"0">>, _} ->
            validate_rpc_request(Elems, Data, Acc, Meta);
        false ->
            validate_rpc_request(Elems, Data, Acc, Meta);
        _ ->
            throw({invalid_params, {required, [Key|also_missing(Elems, Data)]}})
    end;
validate_rpc_request_missing(Stmt, Elems, Key, Info, Data, Acc, Meta) ->
    case lists:keyfind(mandatory,1,Info) of
        {mandatory,true} ->
            %% Mandatory leafs MUST NOT have a default statement
            %% (RFC6020, 7.6.4)
            AlsoMissing = also_missing(Elems, Data),
            throw({invalid_params, {required, [Key|AlsoMissing]}});
        _ when Stmt==leaf ->
            case lists:keyfind(default, 1, Info) of
                {default, _, Def, _} ->
                    Value = convert_elem(Stmt, Key, Def, Info),
                     validate_rpc_request(
                       Elems, Data,
                       [{to_atom(Key), Value, Info}|Acc], Meta);
                false ->
                    validate_rpc_request(Elems, Data, Acc, Meta)
            end;
        _ ->
            %% FIXME: check whether this is in fact the right action for all
            %% non-leaf statements
            validate_rpc_request(Elems, Data, Acc, Meta)
    end.

also_missing(Elems, Env, Data) ->
    also_missing(Elems, Data ++ Env).

also_missing([{Stmt,_,Key,I}|T], Data) ->
    case (req_mandatory(Stmt, I) andalso not_in_data(Key, Data)) of
        true ->
            [Key|also_missing(T, Data)];
        false ->
            also_missing(T, Data)
    end;
also_missing([], _) ->
    [].

req_mandatory(Stmt, I) when Stmt==leaf; Stmt==anyxml ->
    req_mandatory_elem(I);
req_mandatory(Stmt, I) when Stmt==list; Stmt=='leaf-list' ->
    req_non_zero_list(I).

req_mandatory_elem(I) ->
    case lists:keyfind(mandatory, 1, I) of
        {_, _, <<"true">>, _} ->
            true;
        _ ->
            false
    end.

req_non_zero_list(I) ->
    case lists:keyfind('min-elements', 1, I) of
        {_, _, <<"0">>, _} ->
            false;
        false ->
            false;
        _ ->
            true
    end.

not_in_data(Key, Data) ->
    not lists:any(fun({K,_}) ->
                          comp(Key, K)
                  end, Data).

convert_elem(Stmt, Key, Value, Info) ->
    convert_elem(Stmt, Key, Value, lists:keyfind(type,1,Info), Info).

convert_elem(anyxml, _, Value, _, _) ->
    Value;
convert_elem(leaf, Key, Value, Type, _Info) ->
    case yang:check_type(Value, Type) of
        {true, NewVal} ->
            NewVal;
        false ->
            throw({invalid_params, {wrong_type, [Key, Value, Type]}})
    end.

keytake(K, Data) ->
    case keyfind(K, Data) of
        false ->
            false;
        Found ->
            {Found, Data -- [Found]}
    end.

keyfind(A, [H|T]) when is_tuple(H) ->
    K = element(1, H),
    case comp(A,K) of
        true ->
            H;
        false ->
            keyfind(A, T)
    end;
keyfind(_, []) ->
    false.

comp(A, A) -> true;
comp(A, B) when is_binary(A), is_list(B) ->
    binary_to_list(A) == B;
comp(A, B) when is_binary(A), is_atom(B) ->
    A == atom_to_binary(B, latin1);
comp(_, _) ->
    false.

%% ===================================================================
%% data_to_json(Yang, Attrs, Reply) -> JSONTerm
%% ===================================================================
data_to_json([{Stmt, _, Key, Info} = Elem|T], Env, Data)
  when Stmt==leaf; Stmt==anyxml; Stmt==list; Stmt=='leaf-list' ->
    case keyfind(Key, Data) of
        false ->
            case keyfind(Key, Env) of
                Found when is_tuple(Found) ->
                    [data_to_json_(Elem, Env, element(2, Found))
                     | data_to_json(T, Env, Data)];
                false ->
                    case req_mandatory(Stmt, Info) of
                        true ->
                            error({invalid_params,
                                   {required, [Key|
                                               also_missing(T, Env, Data)]}});
                        false ->
                            data_to_json_default(Elem)
                                ++ data_to_json(T, Env, Data)
                    end
            end;
        Found when is_tuple(Found) ->
            [data_to_json_(Elem, Env, element(2, Found))
             | data_to_json(T, Env, Data)]
    end;
data_to_json({Stmt, _, Key, _} = Elem, Env, {Key1, Data})
  when Stmt==leaf; Stmt==anyxml; Stmt==list; Stmt=='leaf-list' ->
    case comp(Key, Key1) of
        true ->
            data_to_json_(Elem, Env, Data);
        false ->
            error({invalid_params, {required, [Key]}})
    end;
data_to_json([_|T], Env, Data) ->
    data_to_json(T, Env, Data);
data_to_json([], _, _) ->
    [].

data_to_json_default({Stmt,_,Key,Info}) when Stmt==leaf; Stmt==anyxml ->
    case lists:keyfind(default, 1, Info) of
        {_, _, Default, _} ->
            [{Key, to_json_type(Default, get_type(Info))}];
        false ->
            []
    end;
data_to_json_default({Stmt,_,Key,_}) when Stmt==list; Stmt=='leaf-list' ->
    [{Key, {array, []}}].

data_to_json_({anyxml, _, Key, _}, _Env, Data) ->
    {Key, Data};
data_to_json_({leaf, _, Key, Info}, _Env, Data) ->
    {Key, to_json_type(Data, get_type(Info))};
data_to_json_({Stmt, _, Key, Info}, Env, Data)
  when Stmt==list; Stmt=='leaf-list' ->
    Data1 = case Data of
                {array, D} when is_list(D) -> D;
                D when is_list(D) -> D;
                _ ->
                    error({invalid_params, {type_error, [Key, Stmt, Data]}})
            end,
    if Stmt=='leaf-list' ->
            Type = get_type(Info),
            {Key, {array, [to_json_type(X, Type) || X <- Data1]}};
       Stmt==list ->
            {Key, {array,
                   lists:map(
                     fun({struct,Elems}) ->
                             {struct, data_to_json(Info, Env, Elems)};
                        (L) when is_list(L) ->
                             {struct, data_to_json(Info, Env, L)};
                        (Other) ->
                             error({invalid_params,
                                    [{type_error,[Key,Info,Other]}]})
                     end, Data1)}}
    end.

get_type(Info) ->
    {type,_,_,_} = lists:keyfind(type, 1, Info).

%% ===================================================================

to_json_rpc(Data, Arg, M) ->
    Imports = imports(Data, Arg),
    Data1 = augment(Data, Imports),
    lists:foldr(
      fun({rpc,_,N,InOut}, Acc) ->
	      Ns = M++":"++binary_to_list(N),
	      [{Ns, mk_rpc_pair(InOut, Ns, Data1, Imports)} | Acc];
	 ({notification,_,N,Elems}, Acc) ->
	      Ns = M++":"++binary_to_list(N),
	      [{Ns, notification(Ns, Elems, Data1, Imports)} | Acc];
	 (_, Acc) ->
	      Acc
      end, [], Data1).

imports(Data, {_Dir,Ext,Opts}) ->
    lists:foldl(
      fun({import,_,F,[{prefix,_,Pfx,_}|_]}, Acc) ->
	      File = binary_to_list(F) ++ Ext,
	      case read(File, Ext, Opts) of
		  {{ok, Y},_} ->
		      case [D || {module,_,N,D} <- Y,
				 N == F] of
			  [ImpData] ->
			      orddict:store(Pfx, ImpData, Acc);
			  [] ->
			      %% error({cannot_import, F, no_such_module})
			      io:fwrite("ERROR: cannot_import ~s (~p)~n",
					[F, no_such_module]),
			      Acc
		      end;
		  {Error,_} ->
		      io:fwrite("ERROR: cannot_import ~s (~p)~n", [F, Error]),
		      Acc
	      end;
	 (_, Acc) ->
	      Acc
      end, orddict:new(), Data).

augment([{augment, _, What, Items}|T], Imports) ->
    case re:split(What, "/") of
	[<<>>, Req, Sub] ->
	    %% we're doing very specific processing here, matching only on what
	    %% we expect to find.
	    case {re:split(Req, ":"), re:split(Sub, ":")} of
		{[Pfx, Method], [Pfx, Part]} ->
		    case [X || X <- orddict:fetch(Pfx, Imports),
				element(3, X) == Method] of
			[{rpc,L,M,InOut}] ->
			    PartAm = binary_to_atom(Part, latin1),
			    case lists:keyfind(PartAm, 1, InOut) of
				{_,_,_,List} = Obj ->
				    [{rpc,L,M,
				      expand_uses(
					lists:keyreplace(PartAm, 1, InOut,
							 setelement(
							   4, Obj, List ++ Items)),
					Pfx)}
				     | augment(T, Imports)];
				_ ->
				    error({cannot_augment, What})
			    end;
			[] ->
			    error({augment_not_found, What})
		    end;
		_ ->
		    error({unfamiliar_augment, What})
	    end;
	_ ->
	    error({unfamiliar_augment, What})
    end;
augment([H|T], Imports) ->
    [H|augment(T, Imports)];
augment([], _) ->
    [].

expand_uses([{Type,L,C,I}|T], Pfx) when Type == input; Type == output ->
    [{Type,L,C,expand_uses_(I, Pfx)} | expand_uses(T, Pfx)];
expand_uses([H|T], Pfx) ->
    [H|expand_uses(T, Pfx)];
expand_uses([], _) ->
    [].

expand_uses_([{uses,Lu,What,Iu} = Uses|T], Pfx) ->
    case binary:match(What, <<":">>) of
	nomatch ->
	    [{uses,Lu,<<Pfx/binary, ":", What/binary>>,Iu}|expand_uses_(T, Pfx)];
	_ ->
	    [Uses|expand_uses_(T, Pfx)]
    end;
expand_uses_([H|T], Pfx) ->
    [H|expand_uses_(T, Pfx)];
expand_uses_([], _) ->
    [].


notification(N, Elems, Data, Imports) ->
    {notification,
     descr(Elems),
     {struct, [{"jsonrpc", "2.0"},
	       {"method", N},
	       {"params", {struct, rpc_params(Elems, Data, Imports)}}]}}.

mk_rpc_pair(InOut, N, Data, Imports) ->
    {_,_,_,I} = lists:keyfind(input, 1, InOut),
    O = case lists:keyfind(output, 1, InOut) of
	    {_,_,_,Ox}  -> Ox;
	    false -> void
	end,
    {descr(InOut),
     {request, {struct, [{"jsonrpc", "2.0"},
			 {"method", N},
			 {"id", ""},
			 {"params", {struct, rpc_params(I, Data, Imports)}}]}},
     {reply, {struct, [{"jsonrpc", "2.0"},
		       {"id", ""},
		       {"result",
			case O of
			    void -> void;
			    _ ->
				{struct, rpc_params(O, Data, Imports)}
			end}]}}}.


rpc_params([{uses,_,G,_}|T], Data, Imports) ->
    {Where, GrpName} = case re:split(G, <<":">>) of
			   [_] ->
			       {Data, G};
			   [Pfx, G1] ->
			       {orddict:fetch(Pfx, Imports), G1}
		       end,
    TypeDefs = [{typedef,L,Type,Def} || {typedef,L,Type,Def} <- Where],
    Data1 = Data ++ TypeDefs,
    case [L || {grouping,_,Grp,L} <- Where,
	       Grp == GrpName] of
	[] ->
	    [{"uses-not-found", binary_to_list(G)} | rpc_params(T, Data1, Imports)];
	[Params] ->
	    rpc_params(log_meta(Params, {grouping, G}), Data1, Imports)
		++ rpc_params(T, Data1, Imports)
    end;
rpc_params([{leaf,_,N,Is}|T], Data, Imports) ->
    [{binary_to_list(N), "", descr(Is), [type(Is,Data,Imports),
					 mandatory(Is)|default(Is)]}
     | rpc_params(T, Data, Imports)];
rpc_params([{anyxml,_,N,Is}|T], Data, Imports) ->
    [{binary_to_list(N), "", descr(Is), [{type,anyxml},
					 mandatory(Is)|default(Is)]}
     | rpc_params(T, Data, Imports)];
rpc_params([{'leaf-list',_,N,Items}|T], Data, Is) ->
    L = binary_to_list(N),
    [{L, {array, [{L, "", descr(Items), [type(Items, Data, Is)]}]},
      descr(Items), [type(Items, Data, Is), mandatory(Items) | default(Items)]}
     | rpc_params(T, Data, Is)];
rpc_params([{list,_,N,Items}|T], Data, Is) ->
    [{binary_to_list(N), {array, [{struct, rpc_params(Items, Data, Is)}]},
      descr(Items), [type(Items, Data, Is), mandatory(Items) | default(Items)]}
     | rpc_params(T, Data, Is)];
rpc_params([{container,_,N,Items}|T], Data, Is) ->
    [{binary_to_list(N), {struct, rpc_params(Items, Data, Is)},
      descr(Items), [type(Items, Data, Is), mandatory(Items) | default(Items)]}
     | rpc_params(T, Data, Is)];
rpc_params([_|T], Data, Is) ->
    rpc_params(T, Data, Is);
rpc_params([], _, _) ->
    [].

log_meta(Params, Info) ->
    [{'$meta', Info}|Params].

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
    ["# Module: ", M, "\n\n", markdown_rpcs(RPCs), "\n\n" | markdown(T)];
markdown([]) ->
    [].

markdown_rpcs([{Not, {notification, Descr, Msg}} | T]) ->
    ["## Notification: ", Not, "\n\n\n", i(0), pp_json(Msg), "\n\n\n",
     case Descr of
	 "" -> "";
	 _ ->
	     [Descr, "\n\n"]
     end,
     markdown_descriptions(Msg), "\n\n" | markdown_rpcs(T)];
markdown_rpcs([{RPC, {Descr, {request, Req}, {reply, Rep}}} | T]) ->
    ["# ", RPC, "\n\n",
     "## Request\n\n\n", i(0), pp_json(Req), "\n\n\n",
     case Descr of
	 "" -> "";
	 _ ->
	     [Descr, "\n\n"]
     end,
     markdown_descriptions(Req), "\n\n",
     "## Reply\n\n\n", i(0), pp_json(Rep), "\n\n\n",
     markdown_descriptions(Rep), "\n\n" | markdown_rpcs(T)];
markdown_rpcs([]) ->
    [].

markdown_descriptions(Msg) ->
    case [X || {_,{_,_}} = X <- collect_descriptions(Msg, orddict:new())] of
	[] -> [];
	[{K,{D,T}}|Tail] ->
	    ["### Elements\n",
	     "**", K, "** ", D,
	     " (**type:** ", type_to_text(T), ")\n\n",
	     [["**", K1, "** ", D1,
	       " (**type:** ", type_to_text(T1), ")\n\n"]
	      || {K1,{D1,T1}} <- Tail],
	     "\n\n"]
    end.

collect_descriptions({struct, L}, Acc) ->
    lists:foldl(fun collect_descriptions/2, Acc, L);
collect_descriptions({array, L}, Acc) ->
    lists:foldl(fun collect_descriptions/2, Acc, L);
collect_descriptions({K,{array,L},D,_}, Acc) ->
    lists:foldl(fun collect_descriptions/2,
		orddict:store(K, {D,<<"array">>}, Acc), L);
collect_descriptions({K,{struct,L},D,_}, Acc) ->
    lists:foldl(fun collect_descriptions/2,
		orddict:store(K, {D,<<"object">>}, Acc), L);
collect_descriptions({K,V,D,T}, Acc) ->
    collect_descriptions(V, orddict:store(K, {D,T}, Acc));
collect_descriptions({_K,V}, Acc) ->
    collect_descriptions(V, Acc);
collect_descriptions(_, Acc) ->
    Acc.

pp_json(Json) ->
    pp_json(Json, 0).

pp_json(void, _) ->
    "\"ok\"";
pp_json({struct, []}, I) ->
    [i(I), "{}\n"];
pp_json({struct, [H|T]}, I) ->
    I1 = I+1,
    ["{\n", i(I1), pp_json(H, I1), [[",\n", i(I1), pp_json(Term,I1)] || Term <- T], "\n", i(I), "}"];
pp_json({array, []}, _I) ->
    ["[]"];
pp_json({array, [H|T]}, I) ->
    I1 = I+1,
    ["[\n", i(I1), pp_json(H, I1), [[",\n", i(I1), pp_json(Term,I1)] || Term <- T], "\n", i(I), "]"];
pp_json(V, _I) when is_binary(V); is_list(V) ->
    io_lib:fwrite("\"~s\"", [V]);
pp_json(V, _I) when is_integer(V) ->
    ["\"", integer_to_list(V), "\""];
pp_json({K,V,_,_}, I) ->
    pp_json_(K, V, I);
pp_json({K,V}, I) ->
    pp_json_(K, V, I).


pp_json_(K,V,I) ->
    Part1 = ["\"", K, "\": "],
%%    I1 = I+1,
    [Part1, pp_json(V, I)].

descr(L) ->
    case lists:keyfind(description, 1, L) of
	false ->
	    "";
	{_, _, B,_} ->
	    binary_to_list(B)
    end.

type(Is, Data, Imports) ->
    case lists:keyfind(type, 1, Is) of
	false ->
	    {type, undefined};
	{type, L, <<"union">>, Ts} ->
	    {type, L, <<"union">>, [type([T1], Data, Imports) ||
				       {type,_,_,_} = T1 <- Ts]};
	{type, _, T, _} = Type ->
	    case binary:split(T, <<":">>) of
		[Pfx, Ts] ->
		    ImpData = orddict:fetch(Pfx, Imports),
		    case [D1 || {typedef,_,T1,D1} <- ImpData, Ts == T1] of
			[_] = Def ->
			    type(Def, ImpData, Imports);
			[] ->
			    Type
		    end;
		_ ->
		    case [D1 || {typedef,_,T1,D1} <- Data, T == T1] of
			[Def|_] ->
			    type(Def, Data, Imports);
			[] ->
			    Type
		    end
	    end
    end.

mandatory(Is) ->
    case lists:keyfind(mandatory, 1, Is) of
	{mandatory, _, Bool, _} ->
	    {mandatory, Bool};
	false ->
	    {mandatory, false}
    end.

default(Is) ->
    case lists:keyfind(default, 1, Is) of
	{default, _, Default, _} ->
	    [{default, Default}];
	false ->
	    []
    end.

descr_type(Is, Data, Imports) ->
    case lists:keyfind(type, 1, Is) of
	false ->
	    <<"untyped">>;
	{type, _, T, _} = Type ->
	    case binary:split(T, <<":">>) of
		[Pfx, Ts] ->
		    ImpData = orddict:fetch(Pfx, Imports),
		    case [D1 || {typedef,_,T1,D1} <- ImpData, Ts == T1] of
			[Def] ->
			    type(Def, ImpData, Imports);
			[] ->
			    Type
		    end;
		_ ->
		    case [D1 || {typedef,_,T1,D1} <- Data, T == T1] of
			[Def|_] ->
			    type(Def, Data, Imports);
			[] ->
			    Type
		    end
	    end
    end.

%% descr_type({type, _, <<"enumeration">>, [{enum,_,_,_} |_] = En}) ->
%%     {enum, [ {E, I} || {enum,_,E,I} <- En] };
%% descr_type({type, _, T, _}) ->
%%     T.

%% descr_type({type, _, <<"enumeration">>, [{enum,_,E,I} |En]}) ->
%%     [ enum_value(I), " (", E, ")" | [ [ " | ", enum_value(I1), " (", E1, ")"]
%% 				     || {enum,_,E1,I1} <- En ] ];
%% descr_type({type, _, <<"boolean">>, _}) ->
%%     "\"1\" (true) | \"0\" (false)";
%% descr_type({type, _, T, _}) ->
%%     T.

type_to_text(B) when is_binary(B) ->
    B;
type_to_text(L) ->
    case lists:keyfind(type, 1, L) of
	false ->
	    [type_to_text_(undefined), "; ", mandatory_to_text(L)];
	T ->
	    [type_to_text_(T), "; ", mandatory_to_text(L)]
    end.

type_to_text_({type, undefined}) ->
    "untyped";
type_to_text_({type, anyxml}) ->
    "XML";
type_to_text_({type, _, <<"enumeration">>, [{enum,_,E,I} | _] = En}) ->
    [ val2txt(I), " (", E, ")" | [ [ " | ", val2txt(I1), " (", E1, ")"]
				   || {enum, _, E1, I1} <- En] ];
type_to_text_({type, _, <<"boolean">>, _}) ->
    "\"1\" (true) | \"0\" (false)";
type_to_text_({type, _, <<"union">>, Ts}) ->
    [ "One of:", [["~n* ", type_to_text(T)] || T <- Ts] ];
type_to_text_({type, _, T, _}) ->
    T;
type_to_text_(T) when is_binary(T) ->
    T.

mandatory_to_text(L) ->
    case lists:keyfind(mandatory, 1, L) of
	false ->
	    "[<em>mandatory: false</em>]";
	{mandatory,B} when is_boolean(B) ->
	    ["[<em>mandatory: ", atom_to_list(B),"</em>]"]
    end.

%% enum_value(I) ->
%%     {value,_,V,_} = lists:keyfind(value, 1, I),
%%     V.
val2txt(I) ->
    {value,_,V,_} = lists:keyfind(value, 1, I),
    ["\"", binary_to_list(V), "\""].

i(I) ->
    lists:duplicate(I*4+4,$\s) .

to_atom(L) when is_list(L) ->
    list_to_atom(L);
to_atom(B) when is_binary(B) ->
    binary_to_atom(B, latin1);
to_atom(A) when is_atom(A) ->
    A.

%% duplicated from yang.erl
find_enum([{enum, _, Key, I}|T], X) ->
    V = get_en_value(I),
    if X == V ->
            [{Key, V}];
       X == Key ->
            [{Key, V}|find_enum(T, X)];
       true ->
            find_enum(T, X)
    end;
find_enum([_|T], X) ->
    find_enum(T, X);
find_enum([], _) ->
    [].

get_en_value(I) ->
    {value, _, V, _} = lists:keyfind(value, 1, I),
    V.

%% ===================================================================
%% EUnit Test Code
%% ===================================================================
-ifdef(TEST).

validate_json_simplest_test_() ->
    Y = testdata("y0.yang", fun y0/0),
    {with, Y,
     [
      fun simplest_rpc_test1/1
     ]}.

validate_json_test_() ->
    Y = testdata("y.yang", fun y1/0),
    {with, Y,
     [
      fun valid_rpc_test/1,
      fun missing_list_test/1
     ]}.

simplest_rpc_test(Y) ->
    {ok, [{a,17,[]}], []} =
        yang_json:validate_request(
          {call, <<"y0:t0">>,
           {struct, [{<<"a">>,17}]}}, Y).

simplest_rpc_test1(Y) ->
    {ok, [{l,[],[{leaf,_,<<"x">>,[{type,_,<<"uint32">>,_}]}]}], []} =
        yang_json:validate_request(
          {call, <<"y0:t1">>,
           {struct, [{<<"l">>,{array, []}}]}}, Y).

valid_rpc_test(Y) ->
    {ok, [
          {a, 17, [{{<<"y">>,<<"foo">>},_,<<"in_a">>,_},
                   {type,_,<<"uint32">>,[]},
                   {mandatory,_,true,[]}]},
          {b, [<<"abc">>], [{type,_,<<"string">>,[]},
                            {'min-elements',_,<<"1">>,[]}]},
          {c, [
               [{x, 1}, {y, <<"abc">>}]
              ], [{leaf,_,<<"x">>,[{{<<"y">>,<<"foo">>},_,<<"in_x">>,_},
                                   {type,_,<<"uint32">>,_}]},
                  {leaf,_,<<"y">>,[{type,_,<<"string">>,_}]}]}
         ], [{{<<"y">>,<<"foo">>},_,<<"input">>,_}]} =
        yang_json:validate_request(
          {call,<<"y:t1">>,
           {struct,[{<<"a">>,17},
                    {<<"b">>, {array, [<<"abc">>]}},
                    {<<"c">>,{array,[
                                     {struct, [{<<"x">>,1},
                                               {<<"y">>,<<"abc">>}
                                              ]}
                                    ]}}
                   ]}
          }, Y).

missing_list_test(Y) ->
    {error, {invalid_params, {required, [<<"b">>]} } } =
        yang_json:validate_request(
          {call,<<"y:t1">>,
           {struct,[{<<"a">>,17},
                    %% missing <<"b">>
                    {<<"c">>,{array,[{struct,[{<<"x">>,1},
                                              {<<"y">>,<<"abc">>}]}]}
                    }]}
          }, Y).

testdata(File, Fn) ->
    {ok, Y} = yang_parser:deep_parse(
                File, [{open_hook, yang:bin_hook([{File, Fn}])}]),
    Y.

enum_test() ->
    File = "e1.yang",
    {ok, [{module,_,<<"e1">>, Y}]} =
        yang_parser:deep_parse(
          File,
          [{open_hook, yang:bin_hook([{File, fun e1/0}])}]),
    [{leaf,_,<<"l">>,I}] = [E || {leaf,_,<<"l">>,_} = E <- Y],
    Type = lists:keyfind(type, 1, I),
    {true, <<"zero">>} = yang:check_type(<<"0">>, Type),
    {true, <<"zero">>} = yang:check_type(0, Type),
    {true, <<"zero">>} = yang:check_type("0", Type),
    {true, <<"then">>} = yang:check_type(<<"3">>, Type),
    {true, <<"then">>} = yang:check_type(<<"then">>, Type),
    {true, <<"3">>} = yang:check_type(<<"4">>, Type),
    <<"zero">> = yang_json:to_json_type(<<"0">>, Type),
    <<"zero">> = yang_json:to_json_type(0, Type),
    <<"zero">> = yang_json:to_json_type("0", Type),
    <<"zero">> = yang_json:to_json_type(<<"zero">>, Type),
    <<"zero">> = yang_json:to_json_type("zero", Type),
    ok.

enum_type_test() ->
    Type = {type,186,<<"enumeration">>,
            [{{<<"$yang">>,<<"origtype">>},186,<<"exo:status-code">>,[]},
             {enum,41,<<"accepted">>,
              [{description,42,
                <<"Operation has been accepted and is in progress.">>,[]},
               {value,43,<<"0">>,[]}]},
             {enum,46,<<"complete">>,
              [{description,47,
                <<"The operation has completed successfully.">>,[]},
               {value,48,<<"1">>,[]}]},
             {enum,51,<<"time-out">>,
              [{description,52,<<"Operation has timed out.">>,[]},
               {value,53,<<"2">>,[]}]},
             {enum,56,<<"device-connected">>,
              [{description,57,
                <<"A connection has been established...">>,[]},
               {value,60,<<"3">>,[]}]},
             {enum,63,<<"device-unknown">>,
              [{description,64,
                <<"The device-id provided with the operation is...">>,[]},
               {value,66,<<"4">>,[]}]},
             {enum,70,<<"device-error">>,
              [{description,71,
                <<"The device encountered an error when ...">>,[]},
               {value,74,<<"5">>,[]}]},
             {enum,77,<<"format-error">>,
              [{description,78,<<"The RPC had an incorrect element ...">>,
                []},
               {value,79,<<"6">>,[]}]},
             {enum,82,<<"value-error">>,
              [{description,83,<<"The RPC had illegal values in ...">>,[]},
               {value,85,<<"7">>,[]}]}]},
    {true, <<"value-error">>} = yang:check_type(7, Type),
    {true, <<"value-error">>} = yang:check_type("7", Type),
    {true, <<"value-error">>} = yang:check_type(<<"7">>, Type),
    ok.

e1() ->
    <<
"module e1 {
  namespace \"http://feuerlabs.com/test\";
  prefix e1;

  leaf l {
    type enumeration {
      enum zero;
      enum one;
      enum then { value 3; }
      enum 3;
    }
  }
}"
>>.

y0() ->
    <<
"module y0 {
  namespace \"http://feuerlabs.com/test\";
  prefix y;

  rpc t0 {
    input {
       leaf a {
         type uint32;
         mandatory true;
       }
    }
  }
  rpc t1 {
    input {
      list l {
        leaf x {
          type uint32;
        }
      }
    }
  }
}"
>>.

y1() ->
    <<
"module y {
  namespace \"http://feuerlabs.com/test\";
  prefix y;

  extension foo {
    argument x;
  }

  rpc t1 {
    input {
       y:foo 'input';
       leaf a {
         y:foo 'in_a';
         type uint32;
         mandatory true;
       }
       leaf-list b {
         type string;
         min-elements 1;
       }
       list c {
         leaf x {
           y:foo 'in_x';
           type uint32;
         }
         leaf y {
           type string;
         }
      }
    }
  }
}"
>>.
-endif.
