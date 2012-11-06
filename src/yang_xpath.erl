-module(yang_xpath).
-compile(export_all).


schema(String, Schema) ->
    schema(String, Schema, Schema).

schema(String, SchemaSub, Schema) ->
    case yang_xpath_parse:parse(yang_xpath_scan:tokens(String)) of
	{ok, Expr} ->
	    eval(Expr, SchemaSub, ensure_root(Schema));
	Error ->
	    Error
    end.

parse(S) ->
    yang_xpath_parse:parse(yang_xpath_scan:tokens(S)).

ensure_root([{module,_,_,_}] = S) ->
    S;
ensure_root([{'$root', _}] = S) ->
    S;
ensure_root(S) ->
    [{'$root', S}].


eval({path, Type, PathExpr}, Sch, Sch0) ->
    path(Type, PathExpr, Sch, Sch0).

path(abs, {step, Tgt}, _Rel, Full) ->
    step(Tgt, Full);
path(abs, {refine, A, B}, _Rel, Full) ->
    path(rel, B, path(abs, A, Full, Full), Full);
path(rel, {step, Tgt}, Rel, _Full) ->
    step(Tgt, Rel);
path(rel, {refine, A, B}, Rel, Full) ->
    path(rel, B, path(rel, A, Rel, Full), Full);
path(union, {A, B}, Rel, Full) ->
    union(eval(A, Rel, Full), eval(B, Rel, Full)).

step(_, []) ->
    [];
step({child, Op, Args}, Set) ->
    apply_pred(Args, lists:flatmap(
		       fun({'$root', Ch}) ->
			       child(Op, Args, Ch);
			  ({_, _, _, Ch}) ->
			       child(Op, Args, Ch)
		       end, Set));
step({descendant_or_self, {node_type,node}, Args} = Instr, Set0) ->
    Set = case Set0 of
	      [{'$root', S}] -> S;
	      _ -> Set0
	  end,
    if Args == [] ->
	    apply_pred(Args, Set);
       true ->
	    apply_pred(
	      Args,
	      lists:flatmap(
		fun({_,_,_,Ch} = X) ->
			case match_args(X, Args) of
			    true ->
				[X|step(Instr, Ch)];
			    false ->
				step(Instr, Ch)
			end
		end, Set))
    end.

child({wildcard, wildcard}, Args, Ch) ->
    if Args == [] -> apply_pred(Args, Ch);
       true ->
	    apply_pred(Args, [X || X <- Ch, match_args(X, Args)])
    end;
child({name, {AbsName, _Pfx, Name}}, Args, Ch) ->
    apply_pred(
      Args,
      [X || {_,_,Id,_} = X <- Ch,
	    (Id =:= Name orelse Id =:= AbsName)
		andalso match_args(X, Args)]).


match_args(_, []) ->
    true;
match_args(_X, Args) ->
    case [A || A <- Args,
	       element(1,A) =/= pred] of
	[] ->
	    true;
	_ ->
	    false
    end.


apply_pred(_, []) ->
    [];
apply_pred(Args, Set) ->
    case [P || {pred,P} <- Args] of
	[] ->
	    Set;
	Ps ->
	    %% can there be more than one predicate?
	    lists:foldl(fun(P, Acc) ->
				pred(P, Acc)
			end, Set, Ps)
    end.

pred({number, N}, Set) ->
    pick_nth(N, Set);
pred({function_call, last, []}, Set) ->
    [lists:last(Set)].

pick_nth(1, [H|_]) ->
    [H];
pick_nth(N, [_|T]) when N > 1 ->
    pick_nth(N-1, T);
pick_nth(_, _) ->
    [].



%% Assume no duplicates in either A or B
union(A, B) ->
    A ++ (B -- A).
