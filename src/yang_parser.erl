%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    YANG parser
%%% @end
%%% Created :  3 Jan 2012 by Tony Rogvall <tony@rogvall.se>

-module(yang_parser).

-export([parse/1, parse/2]).
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

-define(debug, true).

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
