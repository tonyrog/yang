%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    YANG parser
%%% @end
%%% Created :  3 Jan 2012 by Tony Rogvall <tony@rogvall.se>

-module(yang_parser).

-export([parse/1, parse/2]).
-export([fold/3,fold/4]).
-export([get_last_arg/2, get_first_arg/2]).
-export([empty/1, empty/2]).

-export([perf/0, perf/1, perf/3]).
-export([perf_file/0]).
-export([prof/0, prof/1, prof/3]).

-import(lists, [reverse/1]).


perf() ->
    perf([{chunk_size,16*1024}]).
perf(Opts) ->
    perf(perf_file(), Opts, 100).

perf(File,Opts,N) ->
    T0 = os:timestamp(),
    loop(File,Opts,N),
    T1 = os:timestamp(),
    (N/timer:now_diff(T1,T0))*1000000.

perf_file() ->
    filename:join([code:priv_dir(yang),"modules","ietf","RMON-MIB.yang"]).

prof() ->
    prof([{chunk_size,16*1024}]).
prof(Opts) ->
    File = filename:join([code:priv_dir(yang),"modules","ietf",
			  "RMON-MIB.yang"]),
    prof(File, Opts, 1).

prof(File,Opts,N) ->
    T0 = os:timestamp(),
    fprof:trace(start),
    loop(File,Opts,N),
    fprof:trace(stop),
    T1 = os:timestamp(),
    (N/timer:now_diff(T1,T0))*1000000.

loop(_File,_Opts,0) ->
    ok;
loop(File,Opts,I) ->
    {ok,_Ts} = parse(File,Opts),
    loop(File,Opts,I-1).

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

%%
%% @doc
%%    Parse and collect all statements
%% @end
%%
parse(File) ->
    parse(File,[]).

parse(File,Opts) ->
    Fun = fun(Key,Ln,Arg,Acc0,Acc) ->
		  %% TEST!
		  Stmts = reverse(Acc0),
		  case yang_validate:is_valid(Key,Stmts) of
		      true ->
			  ok;
		          %% io:format("~s OK\n", [Key]);
		      false ->
			  io:format("error: ~s invalid\n", [Key])
		  end,
		  {true,[{Key,Ln,Arg,Stmts}|Acc]}
	  end,
    fold(Fun, [], File, Opts).

%%
%% Example: get last statement arg statement
%%
get_last_arg(Stmt,File) ->
    Fun = fun(Stmt0,_Ln,Arg,_,_) when Stmt0 =:= Stmt -> 
		  {true,Arg};
	     (_Key,_Ln,_Arg,Acc,<<>>) -> {true,Acc};
	     (_Key,_Ln,_Arg,<<>>,Acc) -> {true,Acc}
	  end,
    fold(Fun, <<>>, File, []).

get_first_arg(Stmt,File) ->
    Fun = fun(Stmt0,_Ln,Arg,_,_) when Stmt0 =:= Stmt ->
		  {ok,Arg};  %% terminate at first sight
	     (_Key,_Ln,_Arg,_,Acc) -> {true,Acc}
	  end,
    fold(Fun, <<>>, File, []).


%% @doc
%% Fold function Fun over YANG statements in a file
%% @end

fold(Fun,Acc0,File) ->
    fold(Fun,Acc0,File,[]).

fold(Fun,Acc0,File,Opts) ->
    case yang_scan:open(File,Opts) of
	{ok,Scan} ->
	    Res = fold_stmt(Fun,Acc0,Scan),
	    yang_scan:close(Scan),
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
    case yang_scan:next(Scan) of
	{{word,Ln,Stmt},Scan1} ->
	    case yang_scan:next(Scan1) of
		{{string,_,Arg},Scan2} ->
		    fold_stmt_list(Stmt,Ln,Arg,Fun,Acc,Acc0,Scan2,I);
		{{word,_,Arg},Scan2} ->
		    fold_stmt_list(Stmt,Ln,Arg,Fun,Acc,Acc0,Scan2,I);
		{Token={_,_},Scan2} ->
		    Scan3 = yang_scan:push_back(Token,Scan2),
		    fold_stmt_list(Stmt,Ln,[],Fun,Acc,Acc0,Scan3,I);
		eof ->
		    {error,{Ln,"statement ~w not terminated", [Stmt]}};
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
    case yang_scan:next(Scan) of
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
	    {error,{Ln,"statment ~s not terminated", [Key]}};
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


stmt_keyword(Name) ->
    case Name of
	"anyxml" -> 'anyxml';
	"argument" -> 'argument';
	"augment" -> 'augment';
	"base" -> 'base';
	"belongs-to" -> 'belongs-to';
	"bit" -> 'bit';
	"case" -> 'case';
	"choice" -> 'choice';
	"config" -> 'config';
	"contact" -> 'contact';
	"container" -> 'container';
	"default" -> 'default';
	"description" -> 'description';
	"enum" -> 'enum';
	"error-app-tag" -> 'error-app-tag';
	"error-message" -> 'error-message';
	"extension" -> 'extension';
	"deviation" -> 'deviation';
	"deviate" -> 'deviate';
	"feature" -> 'feature';
	"fraction-digits" -> 'fraction-digits';
	"grouping" -> 'grouping';
	"identity" -> 'identity';
	"if-feature" -> 'if-feature';
	"import" -> 'import';
	"include" -> 'include';
	"input" -> 'input';
	"key" -> 'key';
	"leaf" -> 'leaf';
	"leaf-list" -> 'leaf-list';
	"length" -> 'length';
	"list" -> 'list';
	"mandatory" -> 'mandatory';
	"max-elements" -> 'max-elements';
	"min-elements" -> 'min-elements';
	"module" -> 'module';
	"must" -> 'must';
	"namespace" -> 'namespace';
	"notification" -> 'notification';
	"ordered-by" -> 'ordered-by';
	"organization" -> 'organization';
	"output" -> 'output';
	"path" -> 'path';
	"pattern" -> 'pattern';
	"position" -> 'position';
	"prefix" -> 'prefix';
	"presence" -> 'presence';
	"range" -> 'range';
	"reference" -> 'reference';
	"refine" -> 'refine';
	"require-instance" -> 'require-instance';
	"revision" -> 'revision';
	"revision-date" -> 'revision-date';
	"rpc" -> 'rpc';
	"status" -> 'status';
	"submodule" -> 'submodule';
	"type" -> 'type';
	"typedef" -> 'typedef';
	"unique" -> 'unique';
	"units" -> 'units';
	"uses" -> 'uses';
	"value" -> 'value';
	"when" -> 'when';
	"yang-version" -> 'yang-version';
	"yin-element" -> 'yin-element';
	_ -> list_to_binary(Name)   %% save it as binay
    end.

other_keyword(Arg) ->
    case Arg of
	"add" -> 'add';
	"current" -> 'current';
	"delete" -> 'delete';
	"deprecated" -> 'deprecated';
	"false" -> 'false';
	"max" -> 'max';
	"min" -> 'min';
	"not-supported" -> 'not-supported';
	"obsolete" -> 'obsolete';
	"replace" -> 'replace';
	"system" -> 'system';
	"true" -> 'true';
	"unbounded" -> 'unbounded';
	"user" -> 'user';
	_ -> list_to_binary(Arg)
    end.


