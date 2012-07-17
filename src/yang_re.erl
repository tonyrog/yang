%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%%
%%%---- END COPYRIGHT ---------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%     YANG regular expression
%%% @end
%%% Created :  7 Jan 2012 by Tony Rogvall <tony@rogvall.se>

-module(yang_re).

-export([format/1, format/2]).
-export([normalize/1]).
-export([expand/1, expand/2]).

-compile(export_all).

-import(lists, [map/2]).

-type occure() :: {non_neg_integer(), non_neg_integer()|unbounded}.
-type name() :: atom().

-type rexpr0() :: atom() | integer() | binary().

-type rexpr1() :: rexpr0() |
		  {empty} |
		  {sequence,[rexpr1()]} | {all,[rexpr1()]} |
		  {choice,[rexpr1()]} | {closure,rexpr1()} |
		  {pclosure,rexpr1()} | {optional,rexpr1()} |
		  {{choice,name()},[rexpr1()]} |
		  {{closure,name()},rexpr1()} |
		  {{pclosure,name()},rexpr1()}.

-type rexpr2() :: rexpr1() |
		  {rexpr1(),occure()}.

-type rexpr3() :: rexpr2() |
		  {',',[rexpr3()]} | {'&',[rexpr3()]} |
		  {'|',[rexpr3()]} | {'*',rexpr3()} |
		  {'+',rexpr3()} | {'?',rexpr3()}.

-type rexpr() :: rexpr3().


-define(is_occure(N,M),
	( (is_integer((N))) andalso
	  (is_integer((M)) orelse ((X) =:= unbounded)) andalso
	 ((N)=<(M)) )).

-define(is_symbol(X), (is_atom(X) orelse is_binary(X) orelse is_integer(X))).

%%
%% @doc
%%    Normalize a regular expression into "normal" form
%%    X{1,1} =>  X
%%    X{0,0} =>  e
%%    X{0,}  =>  X*
%%    X{1,}  =>  X+
%%    AeB    =>  AB   (sequence,all)
%%    (A)(B) =>  AB
%% @end
%%
-spec normalize(Expr::rexpr3()) -> rexpr2().

normalize(X0) ->
    case X0 of
	{empty} -> X0;
	{re,_}  -> X0;
	{const,_} -> X0;
	X when ?is_symbol(X) -> X;
	{',',Xs} ->
	    normalize({sequence,Xs});
	{'&',Xs} ->
	    normalize({all,Xs});
	{'|',Xs} ->
	    normalize({choice,Xs});
	{'*',X}  ->
	    normalize({closure,X});
	{'+',X}  ->
	    normalize({pclosure,X});
	{'?',X}  ->
	    normalize({optional,X});
	{X,{0,0}} ->
	    case normalize(X) of
		_X2 -> {empty}
	    end;
	{X,{1,1}} ->
	    normalize(X);
	{X,{0,unbounded}} ->
	    normalize({closure,X});
	{X,{0,1}} ->
	    normalize({optional,X});
	{X,{1,unbounded}} ->
	    normalize({pclosure,X});

	{X,{N,M}} when ?is_occure(N,M) ->
	    case normalize(X) of
		{empty} -> {empty};
		Y -> {Y,{N,M}}
	    end;

	{choice,Xs} when is_list(Xs) ->
	    case lists:usort(normalize_list(Xs)) of
		[] ->  {empty};
		[Y] -> Y;
		Ys -> {choice,Ys}
	    end;

	{Op,Xs} when is_atom(Op), is_list(Xs) ->
	    case delete({empty},normalize_list(Xs)) of
		[] ->  {empty};
		[Y] -> Y;
		Ys -> {Op,Ys}
	    end;
	{Op,X} when is_atom(Op) ->
	    case normalize(X) of
		[] ->  {empty};
		[Y] -> Y;
		Ys -> {Op,Ys}
	    end
    end.

normalize_list([X|Xs]) ->
    [ normalize(X) | normalize_list(Xs)];
normalize_list([]) ->
    [].

delete(X,[X|Xs]) -> delete(X,Xs);
delete(X,[Y|Xs]) -> [Y|delete(X,Xs)];
delete(_X,[]) -> [].

%% @doc
%%    Expand a normalized expression
%% @end
-spec expand(Expr::rexpr(),Lookup::(fun((rexpr0()) -> {boolean(),rexpr()}))) ->
		    rexpr().

expand(Expr) ->
    expand(Expr, fun(X) -> {false,X} end).

expand(Expr,Lookup) ->
    case Expr of
	{empty} -> Expr;
	{re,_}  -> Expr;
	{const,_} -> Expr;
	X when ?is_symbol(X) ->
	    case Lookup(X) of
		{false,X1} ->
		    X1;
		{true,Expr1} ->
		    expand(Expr1,Lookup)
	    end;
	{choice,List} when is_list(List) ->
	    case expand_list(List,Lookup) of
		[] -> {empty};
		[A] -> A;
		List1 -> {choice,merge(choice, List1)}
	    end;
	{sequence,List} when is_list(List) ->
	    case delete({empty},expand_list(List,Lookup)) of
		[] -> {empty};
		[A] -> A;
		List1 -> {sequence,merge(sequence,List1)}
	    end;
	{all,List} when is_list(List) ->
	    case delete({empty},expand_list(List,Lookup)) of
		[] -> {empty};
		[A] -> A;
		L1 -> {all,merge(all,L1)}
	    end;
	{closure,X} ->
	    case expand(X,Lookup) of
		{empty} -> {empty};
		{closure,X1} -> {closure,X1};
		X1 -> {closure,X1}
	    end;
	{pclosure,X} ->
	    case expand(X,Lookup) of
		{empty} -> {empty};
		X1 -> {pclosure,X1}
	    end;
	{optional,X} ->
	    case expand(X,Lookup) of
		{empty} -> {empty};
		X1 -> {optional,X1}
	    end;

	{X,{N,M}} when ?is_occure(N,M) ->
	    %% maybe keep occure? if using pcre!
	    X1 = expand(X,Lookup),
	    {X1,{N,M}}
%%	    if M =:= unbounded ->
%%		    {sequence,lists:duplicate(N,X1)++[{closure,X1}]};
%%	       true ->
%%		    {choice,map(fun(I) ->
%%					{sequence,lists:duplicate(I,X1)}
%%				end, lists:seq(N,M))}
%%	    end
    end.

expand_list([X|Xs],Lookup) ->
    [ expand(X,Lookup) | expand_list(Xs,Lookup)];
expand_list([], _Lookup) ->
    [].

%% merge sublist into list if they are of the right Type
merge(Type, [{Type,List}|T]) ->
    merge(Type, List ++ T);
merge(Type, [H|T]) ->
    [H | merge(Type,T)];
merge(_Type, []) ->
    [].

%% @doc
%%    Format an regular expression
%% @end
-spec format(Expr::rexpr3()) -> iolist().

format(X) ->
    format(X, fun(Y) -> Y end).

format(X,F) ->
    case X of
	{const,String} -> %% FIXME: escape when needed !!! :-)
	    String;
	{re,REString} ->
	    ["(",REString,")"]; %% pre-formated expression
	{choice,L} ->
	    ["(",format_list(L,F,"|"),")"];
	{sequence,L} ->
	    ["(",format_list(L,F,""),")"];
	{closure,X1} ->
	    ["(",format(X1,F),")*"];
	{pclosure,X1} ->
	    ["(",format(X1,F),")+"];
	{optional,X1} ->
	    ["(",format(X1,F),")?"];
	{empty} -> [];
	{X1,{N,M}} when ?is_occure(N,M) ->
	    if M =:= unbounded ->
		    ["(",format(X1,F),")","{",integer_to_list(N),",}"];
	       N =:= M ->
		    ["(",format(X1,F),")","{",integer_to_list(N),"}"];
	       true ->
		    ["(",format(X1,F),")","{",
		     integer_to_list(N),",",integer_to_list(M),"}"]
	    end;
	X1 when is_atom(X1) ->    F(X);
	X1 when is_integer(X1) -> X1
    end.

format_list([H],F,_Sep) ->
    [format(H,F)];
format_list([H|T],F,Sep) ->
    [format(H,F),Sep | format_list(T,F,Sep)];
format_list([],_F,_Sep) ->
    [].
