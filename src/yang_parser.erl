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
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @doc
%%%    YANG parser
%%% @end
%%% Created :  3 Jan 2012 by Tony Rogvall <tony@rogvall.se>

-module(yang_parser).

-export([parse/1, parse/2]).
-export([deep_parse/1, deep_parse/2]).
-export([validate/1, validate/2]).
-export([fold/3,fold/4]).
-export([get_last_arg/2, get_first_arg/2]).
-export([empty/1, empty/2]).

%% Test & Performance
-export([perf/0, perf/1]).
-export([perf_valid/0, perf_valid/1]).
-export([perf2/0, perf2/1]).
-export([perf2_valid/0, perf2_valid/1]).
-export([perf_file/1]).
-export([prof/0, prof/1, prof/4]).

-export([perf/4]).


-import(lists, [reverse/1]).

-include_lib("kernel/include/file.hrl").

-include("../include/yang_types.hrl").

%-define(debug, true).

-ifdef(debug).
-define(dbg(F,A), io:format((F),(A))).
-else.
-define(dbg(F,A), ok).
-endif.

-type option() :: {chunk_size, pos_integer()} | {atom(),any()}.

-define(YANG_SCAN, yang_scan_nif).
%%
%% @doc
%%    Parse and collect all statements
%% @end
%%
-spec parse(File::string()) ->
		   {ok,[yang_statement()]} |
		   {error,any()}.

parse(File) ->
    parse(File,[]).

-spec parse(File::string(), Opts::[option()]) ->
		   {ok,[yang_statement()]} |
		   {error,any()}.

parse(File,Opts) ->
    Fun = fun(Key,Ln,Arg,Acc0,Acc) ->
		  Stmts = reverse(Acc0),
		  Element = {Key,Ln,Arg,Stmts},
		  {true,[Element|Acc]}
	  end,
    fold(Fun, [], File, Opts).

deep_parse(File) ->
    deep_parse(File, []).

deep_parse(File, Opts) ->
    case parse(File, Opts) of
	{ok, Yang} ->
	    expand(Yang, Opts);
	{error, _} = E ->
	    E
    end.

expand([{Type,L,M,Data}], Opts) when Type==module; Type==submodule ->
    try begin
	    {NewData, _Types} = expand_module(Data, M, Opts),
	    {ok, [{Type,L,M,NewData}]}
	end
    catch
	error:E ->
	    {error, {E, erlang:get_stacktrace()}};
	throw:T ->
	    {error, T}
    end.

expand_module(Data, M, Opts) ->
    Imports = imports(Data, Opts),
    Data1 = submodules(Data, M, Opts),
    Data2 = expand_uses(Data1, Imports),
    Typedefs = [{T, lists:keyfind(type,1,Def)} ||
		   {typedef, _, T, Def} <- Data2],
    Data3 = expand_elems_(Data2, M, Typedefs, Imports),
    {Data3, Typedefs}.

imports(Data, Opts) ->
    lists:foldl(
      fun({import,L,M,IOpts}, Dict) ->
	      try import_(M, IOpts, Opts, L, Dict)
	      catch
		  {error, E} ->
		      throw({E, [import, L, M]})
	      end;
	 (_, Dict) ->
	      Dict
      end, orddict:new(), Data).

import_(M, IOpts, Opts, L, Dict) ->
    {Yi, Types} = parse_expand(<<M/binary, ".yang">>, L, Opts),
    Prefix = case lists:keyfind(prefix,1,IOpts) of
		 {prefix,_,P,_} -> P;
		 false ->
		     case lists:keyfind(prefix,1,Yi) of
			 {prefix,_,P1,_} -> P1;
			 false -> M
		     end
	     end,
    orddict:store(Prefix, {M, Yi, Types}, Dict).


parse_expand(F, L, Opts) ->
    case parse(F, Opts) of
	{ok, [{Mod, _, Name, Data}]} when Mod==module; Mod==submodule ->
	    expand_module(Data, Name, Opts);
	{error, Error} ->
	    error({Error, [import, L, F]})
    end.

submodules(Data, M, Opts) ->
    {Res, _} =
	lists:mapfoldl(
	  fun({include, L, SubM, IOpts}, Visited) ->
		  case lists:member(SubM, Visited) of
		      true ->
			  error({include_loop, {L, [SubM|Visited]}});
		      false ->
			  Data1 = include_submodule(SubM, IOpts, Opts),
			  {Data1, [SubM|Visited]}
		  end;
	     (Other, Acc) ->
		  {[Other], Acc}
	  end, [M], Data),
    lists:flatten(Res).

include_submodule(M, IOpts, Opts) ->
    File = case lists:keyfind(revision_date, 1, IOpts) of
	       {'revision_date', _, D, _} ->
		   <<M/binary, "@", D/binary, ".yang">>;
	       false ->
		   <<M/binary, ".yang">>
	   end,
    case deep_parse(File, Opts) of
	{ok, [{submodule, _, _, Data}]} ->
	    Data;
	Other ->
	    error({include_error, [M, IOpts, Other]})
    end.

expand_uses(Data, Imports) ->
    expand_uses(Data, Imports, Data).

expand_uses([{uses,L,U,Ou}|T], Imports, Yang) ->
    Found =
	case binary:split(U, <<":">>) of
	    [UName] ->
		%% FIXME! If no prefix, we should really try to find the
		%% 'closest' match - which seems like a vague concept.
		%% Let's assume that there will be only one match.
		find_grouping(UName, <<>>, Yang, L);
	    [Pfx, UName] ->
		case orddict:find(Pfx, Imports) of
		    {ok, {_, Yi, _}} ->
			find_grouping(UName, Pfx, Yi, L);
		    error ->
			throw({unknown_prefix, [uses, L, U]})
		end
	end,
    Refined = refine(Found, Ou),
    expand_uses(Refined, Imports, Yang) ++ expand_uses(T, Imports, Yang);
expand_uses([{Elem,L,Nm,Data}|T], Imports, Yang) ->
    [{Elem, L, Nm, expand_uses(Data, Imports, Yang)}|
     expand_uses(T, Imports, Yang)];
expand_uses([], _, _) ->
    [].

expand_elems_(Data, M, Typedefs, Imports) ->
    expand_elems_(Data, M, Data, Typedefs, Imports).

expand_elems_([{type,L,Type,[]} = Elem|T], M, Yang, Typedefs, Imports) ->
    case builtin_type(Type) of
	true ->
	    [Elem|expand_elems_(T, M, Yang, Typedefs, Imports)];
	false ->
	    ?dbg("expand_type(~p, ... ~p)~n", [Type, Typedefs]),
	    {NewType,Def} = expand_type(Type, M, L, Typedefs, Imports),
	    [{type,L,NewType,Def}|expand_elems_(T, M, Yang, Typedefs, Imports)]
    end;
expand_elems_([{Elem,L,Name,Data}|T], M, Yang, Typedefs, Imports) ->
    [{Elem,L,Name,fix_expanded_(
		    expand_elems_(Data, M, Yang, Typedefs, Imports))}
     | expand_elems_(T, M, Yang, Typedefs, Imports)];
expand_elems_([], _, _, _, _) ->
    [].

builtin_type(<<"binary"             >>) -> true;
builtin_type(<<"bits"               >>) -> true;
builtin_type(<<"boolean"            >>) -> true;
builtin_type(<<"decimal64"          >>) -> true;
builtin_type(<<"empty"              >>) -> true;
builtin_type(<<"enumeration"        >>) -> true;
builtin_type(<<"identityref"        >>) -> true;
builtin_type(<<"instance-identifier">>) -> true;
builtin_type(<<"int8"               >>) -> true;
builtin_type(<<"int16"              >>) -> true;
builtin_type(<<"int32"              >>) -> true;
builtin_type(<<"int64"              >>) -> true;
builtin_type(<<"leafref"            >>) -> true;
builtin_type(<<"string"             >>) -> true;
builtin_type(<<"uint8"              >>) -> true;
builtin_type(<<"uint16"             >>) -> true;
builtin_type(<<"uint32"             >>) -> true;
builtin_type(<<"uint64"             >>) -> true;
builtin_type(<<"union"              >>) -> true;

builtin_type(<<"object"             >>) -> true;
builtin_type(<<"timestamp"          >>) -> true;
builtin_type(_) -> false.

expand_type(Type, M, L, Typedefs, Imports) ->
    case binary:split(Type, <<":">>) of
	[Pfx, T] ->
	    case orddict:find(Pfx, Imports) of
		{ok, {_, _, Types}} ->
		    case lists:keyfind(T, 1, Types) of
			{_, {type,_,NewType,Def}} ->
			    {NewType, Def};
			false ->
			    throw({unknown_type, [M, L, Type]})
		    end;
		error ->
		    throw({unknown_type, [M, L, Type]})
	    end;
	[_] ->
	    case lists:keyfind(Type, 1, Typedefs) of
		{_, {type,_,NewType,Def}} ->
		    {NewType, Def};
		false ->
		    throw({unknown_type, [M, L, Type]})
	    end
    end.

%% Quick fix to avoid having multiple 'description' elements after expanding
%% 'uses' entries. This costs cpu cycles, of course. Should we even bother?
fix_expanded_([{description,_,_,_} = D|T]) ->
    [D | [E || E <- T, element(1,E) =/= description]];
fix_expanded_([H|T]) ->
    [H|fix_expanded_(T)];
fix_expanded_([]) ->
    [].


find_grouping(G, Pfx, Yang, L) ->
    case [Dg || {grouping,_,G1,Dg} <- Yang, G1 =:= G] of
	[] ->
	    throw({unknown_grouping, [uses,L,prefixed_name(Pfx, G)]});
	[Data] ->
	    Data
    end.

prefixed_name(<<>>, N) -> N;
prefixed_name(Pfx, N ) -> <<Pfx/binary, ":", N/binary>>.


refine(Elems, Opts) ->
    lists:foldr(fun refine_instr/2, Elems, Opts).

refine_instr({Op,_,E,Items}, Elems)
  when Op == refine; Op == augment ->
    ENames = binary:split(E, <<$/>>, [global]),
    refine_list(Op, ENames, Items, Elems);
refine_instr(_, Elems) ->
    Elems.

refine_list(refine, [], Items, EOpts) ->
    refine_(Items, EOpts);
refine_list(augment, [], Items, []) ->
    Items;
refine_list(augment, [], Items, EOpts) ->
    EOpts ++ Items;
refine_list(Op, [<<>>|T], Items, EOpts) ->
    refine_list(Op, T, Items, EOpts);
refine_list(Op, [EName|T], Items, EOpts) ->
    case lists:keyfind(EName, 3, EOpts) of
        Elem when is_tuple(Elem) ->
            NextEOpts = element(4, Elem),
            NewEOpts = refine_list(Op, T, Items, NextEOpts),
            NewElem = setelement(4, Elem, NewEOpts),
            lists:keyreplace(EName, 3, EOpts, NewElem);
        false ->
            throw({refine_error, Op, EName})
    end.

refine_([{K,_,_,_} = H|T], Opts) ->
    refine_(T, lists:keystore(K, 1, Opts, H));
refine_([], Opts) ->
    Opts.

%%
%% @doc
%%    Parse and validate and collect all statements
%% @end
%%
-spec validate(File::string()) ->
		      {ok,[yang_statement()]} |
		      {error,any()}.

validate(File) ->
    validate(File,[]).

-spec validate(File::string(), Opts::[option()]) ->
		      {ok,[yang_statement()]} |
		      {error,any()}.

validate(File,Opts) ->
    Fun = fun(Key,Ln,Arg,Acc0,Acc) ->
		  Stmts = reverse(Acc0),
		  Context = {Key,File,Ln,Opts},
		  case yang_validate:is_valid(Context,Arg,Stmts) of
		      true ->
			  ok;
		      false ->
			  io:format("~s:~w: '~s' invalid statement\n",
				    [File,Ln,Key])
		  end,
		  Element = {Key,Ln,Arg,Stmts},
		  {true,[Element|Acc]}
	  end,
    fold(Fun, [], File, Opts).

%% @doc
%%     Get last statement arg statement (Example)
%% @end
-spec get_last_arg(Stmt::yang_stmt_arg(), File::string()) ->
			  {ok,yang_stmt_arg()} |
			  {error,any()}.

get_last_arg(Stmt,File) ->
    Fun = fun(Stmt0,_Ln,Arg,_,_) when Stmt0 =:= Stmt ->
		  {true,Arg};
	     (_Key,_Ln,_Arg,Acc,<<>>) -> {true,Acc};
	     (_Key,_Ln,_Arg,<<>>,Acc) -> {true,Acc}
	  end,
    fold(Fun, <<>>, File, []).

%% @doc
%%     Get first statement arg statement (Example)
%% @end
-spec get_first_arg(Stmt::yang_stmt_arg(), File::string()) ->
			   {ok,yang_stmt_arg()} |
			   {error,any()}.

get_first_arg(Stmt,File) ->
    Fun = fun(Stmt0,_Ln,Arg,_,_) when Stmt0 =:= Stmt ->
		  {ok,Arg};  %% terminate at first sight
	     (_Key,_Ln,_Arg,_,Acc) -> {true,Acc}
	  end,
    fold(Fun, <<>>, File, []).

%% @doc
%%    Fold function Fun over YANG statements in a file
%%    Function callback return values:
%%       {true, Acc'}   - continue with Acc1
%%       true           - continue with Acc
%%       {ok, Reply}    - reply {ok,Reply}
%%       {error,Reason} - reply {error,Reason}
%%
%% @end
-spec fold(Fun::(fun((Key::yang_stmt_name(),Ln::integer(),
		      Arg::yang_stmt_arg(),
		      Acc0::any(), Acc::any()) ->
			    {true,any()} | true | {ok,any()} | {error,any()})),
	   Acc0::any(), File::string()) ->
		  {ok,any()} | {error,any()}.

fold(Fun,Acc0,File) ->
    fold(Fun,Acc0,File,[]).

fold(Fun,Acc0,File,Opts) ->
    case ?YANG_SCAN:open(File,Opts) of
	{ok,Scan} ->
	    Res = fold_stmt(Fun,Acc0,Scan),
	    ?YANG_SCAN:close(Scan),
	    Res;
	Error ->
	    Error
    end.

%%
%% Fold over stmt
%%
%% Fun(Key,Ln,Arg,Acc0, Acc) ->
%%   {true, Acc'}   - continue with Acc1
%%   true           - continue with Acc
%%   {ok, Reply}    - reply {ok,Reply}
%%   {error,Reason} - reply {error,Reason}
%%

fold_stmt(Fun,Acc0,Scan) ->
    fold_stmt(Fun,Acc0,Acc0,Scan,0).

fold_stmt(Fun,Acc,Acc0,Scan,I) ->
    case ?YANG_SCAN:next(Scan) of
	{{word,Ln,Stmt},Scan1} ->
	    case ?YANG_SCAN:next(Scan1) of
		{{string,_,Arg},Scan2} ->
		    fold_stmt_list(Stmt,Ln,Arg,Fun,Acc,Acc0,Scan2,I);
		{{word,_,Arg},Scan2} ->
		    fold_stmt_list(Stmt,Ln,Arg,Fun,Acc,Acc0,Scan2,I);
		{Token={_,_},Scan2} ->
		    Scan3 = ?YANG_SCAN:push_back(Token,Scan2),
		    fold_stmt_list(Stmt,Ln,[],Fun,Acc,Acc0,Scan3,I);
		eof ->
		    {error,{Ln,"statement ~s not terminated", [Stmt]}};
		Error ->
		    Error
	    end;
	{string,Ln,Stmt} ->
	    {error,{Ln,"string ~s not expected",[Stmt]}};
	{{'}',_Ln},Scan2} when I > 0 ->
	    {stmt_list,Scan2,Acc};
	eof when I =:= 0 ->
	    {ok,Acc};
	{{T,Ln},_Scan2} when is_atom(T) ->
	    {error, {Ln,"token ~w not expected", [T]}};
	Error ->
	    Error
    end.

fold_stmt_list(Key,Ln,Arg,Fun,Acc,Acc0,Scan,I) ->
    case ?YANG_SCAN:next(Scan) of
	{{';',_Ln},Scan1} ->
	    case callback(Fun,Key,Ln,Arg,Acc0,Acc) of
		{true,Acc1} ->
		    fold_stmt(Fun,Acc1,Acc0,Scan1,I);
		true ->
		    fold_stmt(Fun,Acc,Acc0,Scan1,I);
		Result ->
		    Result
	    end;
	{{'{',_Ln},Scan1} ->
	    case fold_stmt(Fun,Acc0,Acc0,Scan1,I+1) of
		{stmt_list,Scan2,Acc2} ->
		    case callback(Fun,Key,Ln,Arg,Acc2,Acc) of
			{true,Acc1} ->
			    fold_stmt(Fun,Acc1,Acc0,Scan2,I);
			true ->
			    fold_stmt(Fun,Acc,Acc0,Scan2,I);
			Result ->
			    Result
		    end;
		Result ->  %% ok,error
		    Result
	    end;
	eof ->
	    {error,{Ln,"statement ~s not terminated", [Key]}};
	{Token={word,Ln1,_}, _Scan1} ->
	    {error,{Ln1,"syntax error near ~w",[Token]}};
	{Token={string,Ln1,_}, _Scan1} ->
	    {error,{Ln1,"syntax error near ~w",[Token]}};
	Error ->
	    Error
    end.

callback(Fun,Key,Ln,Arg,Acc0,Acc) ->
    KeyWord = stmt_keyword(Key),
    ArgWord = other_keyword(Arg),
    Fun(KeyWord,Ln,ArgWord,Acc0,Acc).

stmt_keyword(Bin) ->
    case Bin of
	<<"anyxml">> -> 'anyxml';
	<<"argument">> -> 'argument';
	<<"augment">> -> 'augment';
	<<"base">> -> 'base';
	<<"belongs-to">> -> 'belongs-to';
	<<"bit">> -> 'bit';
	<<"case">> -> 'case';
	<<"choice">> -> 'choice';
	<<"config">> -> 'config';
	<<"contact">> -> 'contact';
	<<"container">> -> 'container';
	<<"default">> -> 'default';
	<<"description">> -> 'description';
	<<"enum">> -> 'enum';
	<<"error-app-tag">> -> 'error-app-tag';
	<<"error-message">> -> 'error-message';
	<<"extension">> -> 'extension';
	<<"deviation">> -> 'deviation';
	<<"deviate">> -> 'deviate';
	<<"feature">> -> 'feature';
	<<"fraction-digits">> -> 'fraction-digits';
	<<"grouping">> -> 'grouping';
	<<"identity">> -> 'identity';
	<<"if-feature">> -> 'if-feature';
	<<"import">> -> 'import';
	<<"include">> -> 'include';
	<<"input">> -> 'input';
	<<"key">> -> 'key';
	<<"leaf">> -> 'leaf';
	<<"leaf-list">> -> 'leaf-list';
	<<"length">> -> 'length';
	<<"list">> -> 'list';
	<<"mandatory">> -> 'mandatory';
	<<"max-elements">> -> 'max-elements';
	<<"min-elements">> -> 'min-elements';
	<<"module">> -> 'module';
	<<"must">> -> 'must';
	<<"namespace">> -> 'namespace';
	<<"notification">> -> 'notification';
	<<"ordered-by">> -> 'ordered-by';
	<<"organization">> -> 'organization';
	<<"output">> -> 'output';
	<<"path">> -> 'path';
	<<"pattern">> -> 'pattern';
	<<"position">> -> 'position';
	<<"prefix">> -> 'prefix';
	<<"presence">> -> 'presence';
	<<"range">> -> 'range';
	<<"reference">> -> 'reference';
	<<"refine">> -> 'refine';
	<<"require-instance">> -> 'require-instance';
	<<"revision">> -> 'revision';
	<<"revision-date">> -> 'revision-date';
	<<"rpc">> -> 'rpc';
	<<"status">> -> 'status';
	<<"submodule">> -> 'submodule';
	<<"type">> -> 'type';
	<<"typedef">> -> 'typedef';
	<<"unique">> -> 'unique';
	<<"units">> -> 'units';
	<<"uses">> -> 'uses';
	<<"value">> -> 'value';
	<<"when">> -> 'when';
	<<"yang-version">> -> 'yang-version';
	<<"yin-element">> -> 'yin-element';
	_ ->
	    case binary:split(Bin, <<":">>) of
		[Prefix,Name] ->
		    {Prefix,Name};
		[Name] ->
		    Name
	    end
    end.


other_keyword(Bin) ->
    case Bin of
	<<"add">> -> 'add';
	<<"current">> -> 'current';
	<<"delete">> -> 'delete';
	<<"deprecated">> -> 'deprecated';
	<<"false">> -> 'false';
	<<"max">> -> 'max';
	<<"min">> -> 'min';
	<<"not-supported">> -> 'not-supported';
	<<"obsolete">> -> 'obsolete';
	<<"replace">> -> 'replace';
	<<"system">> -> 'system';
	<<"true">> -> 'true';
	<<"unbounded">> -> 'unbounded';
	<<"user">> -> 'user';
	_ -> Bin
    end.

%%
%% Test / Performance
%%

perf() ->
    perf([{chunk_size,16*1024}]).
perf(Opts) ->
    perf(perf_file(1), Opts, 100, fun parse/2).

perf_valid() ->
    perf_valid([{chunk_size,16*1024}]).
perf_valid(Opts) ->
    perf(perf_file(1), Opts, 100, fun validate/2).

perf2() ->
    perf2([{chunk_size,16*1024}]).
perf2(Opts) ->
    perf(perf_file(2), Opts, 200, fun parse/2).

perf2_valid() ->
    perf2_valid([{chunk_size,16*1024}]).
perf2_valid(Opts) ->
    perf(perf_file(2), Opts, 200, fun validate/2).

perf(File,Opts,N,F) ->
    {ok,Info} = file:read_file_info(File),
    garbage_collect(),
    {reductions,R0} = process_info(self(), reductions),
    {garbage_collection,G0} = process_info(self(),garbage_collection),
    T0 = os:timestamp(),
    loop(File,Opts,N,F),
    T1 = os:timestamp(),
    {reductions,R1} = process_info(self(), reductions),
    {garbage_collection,G1} = process_info(self(),garbage_collection),
    Td = timer:now_diff(T1,T0),
    io:format("Reductions = ~w\n", [R1-R0]),
    io:format("GC(minor) = ~w\n", [proplists:get_value(minor_gcs,G1) -
				       proplists:get_value(minor_gcs,G0)]),
    io:format("MB/s = ~w\n", [(((Info#file_info.size*N)/(1024*1024))/Td)*1000000]),
    (N/Td)*1000000.

perf_file(1) ->
    filename:join([code:priv_dir(yang),"modules","ietf","RMON-MIB.yang"]);
perf_file(2) ->
    filename:join([code:priv_dir(yang),"modules","ietf","ietf-ipfix-psamp.yang"]).


prof() ->
    prof([{chunk_size,16*1024}]).
prof(Opts) ->
    File = filename:join([code:priv_dir(yang),"modules","ietf",
			  "RMON-MIB.yang"]),
    prof(File, Opts, 1, fun parse/2).

prof(File,Opts,N,F) ->
    T0 = os:timestamp(),
    fprof:trace(start),
    loop(File,Opts,N,F),
    fprof:trace(stop),
    T1 = os:timestamp(),
    (N/timer:now_diff(T1,T0))*1000000.

loop(_File,_Opts,0,_F) ->
    ok;
loop(File,Opts,I,F) ->
    {ok,_Ts} = F(File,Opts),
    loop(File,Opts,I-1, F).

%%
%% Scan everything collect nothing
%%
empty(File) ->
    empty(File,[]).

empty(File,Opts) ->
    Fun = fun(_Key,_Ln,_Arg,_,Acc) ->
		  {true,Acc}
	  end,
    fold(Fun, [], File, Opts).
