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
%%%     YANG validator
%%% @end
%%% Created :  4 Jan 2012 by Tony Rogvall <tony@rogvall.se>

-module(yang_validate).

-compile(export_all).

-export([rule/1]).
-export([is_valid/3]).
-import(lists, [map/2]).

-include("../include/yang_types.hrl").


-record(ystmt,{name,min_occure=0,max_occure=0}).

-define(Y(Min,Max,Name),
	#ystmt{name=(Name),min_occure=(Min),max_occure=(Max)}).

-define(ALL(MinOccure,Name,Elements),   {all,Name,MinOccure,Elements}).
-define(SEQUENCE(Name,Elements),        {seq,Name,Elements}).
-define(CHOICE(MinOccure,Name,Elements),{choice,Name,MinOccure,Elements}).
-define(UNKNOWN, '$unknown').
%% -include("../include/yang.hrl").


%% @doc
%%    Generate a yang.hrl candiate with all data definitions
%% @end
generate_yang_hrl() ->
    {ok,Fd} = file:open("yang-1.hrl", [write]),
    try generate_yang_hrl(Fd) of
	Res -> Res
    after
	file:close(Fd)
    end.

generate_yang_hrl(Fd) ->
    {{YYYY,M,Day},_} = calendar:now_to_local_time(os:timestamp()),
    io:format(Fd, "%%% @author Tony Rogvall <tony@rogvall.se>\n",[]),
    io:format(Fd, "%%% @copyright (C) 2012, Tony Rogvall\n", []),
    io:format(Fd, "%%% @doc\n", []),
    io:format(Fd, "%%%    YANG definitions\n", []),
    io:format(Fd, "%%% @end\n", []),
    io:format(Fd, "%%% Created :  ~w ~s ~4..0w by generage_yang_hrl\n",
	      [Day,month(M),YYYY]),
    io:format(Fd, "-ifndef(__YANG_HRL__).\n",[]),
    io:format(Fd, "-define(__YANG_HRL__, true).\n", []),
    lists:foreach(
      fun(Stmt) ->
	      generate_yang_record(Fd, Stmt)
      end, statements()),
    io:format(Fd, "-endif.\n",[]).

month(I) ->
    element(I,{"Jan","Feb","Mar","Apr","May","Jun",
	       "Jul","Aug","Sep","Oct","Nov","Dec"}).

%% @doc
%%    Given a statement emit a record that describe that statement
%%    as good as possible.
%% @end
%%
generate_yang_record(Fd, Name) ->
    %% io:format(user, "%% making record ~s\n", [Name]),
    {_Name1,Arg1,Rule} = rule(Name),
    %% generate record definition from statement rule
    Fs = case Rule of
	     [] ->
		 "";
	     ?SEQUENCE(_,Elems) ->
		 format_record_fields(groups(Elems));
	     ?ALL(_,_,Elems) ->
		 format_record_fields(groups(Elems));
	     ?CHOICE(_,_,Elems) ->
		 Elems1 = lists:usort(groups(Elems)),
		 format_record_fields(Elems1)
	 end,
    ArgValue = case expand_arg(Arg1) of
		   {const,Const} ->
		       Const;
		   Expanded ->
		       REString = yang_re:format(Expanded),
		       case re:compile(REString) of
			   {ok,_Pattern} ->
			       "";
			   {error,Reason} ->
			       io:format("~s: arg=~w, ~p\n",
					 [Name,Arg1,Reason]),
			       ""
		       end
	       end,
    io:format(Fd, "%%\n%% ~s\n%%\n-record('yang-~s',{", [Name,Name]),
    case Fs of
	"" ->
	    io:format(Fd, " arg=~p", [ArgValue]);
	_ ->
	    io:format(Fd,"\n  arg=~p,\n~s", [ArgValue,Fs])
    end,
    io:format(Fd, " }).\n\n",[]).


%% pick out "toplevel" named groups or items
groups([?ALL(_,none,Elems) | Fs]) ->  groups(Elems) ++ groups(Fs);
groups([?ALL(_,Name,_) | Fs]) -> [{Name,0,n} | groups(Fs)];
groups([?SEQUENCE(none,Elems)|Fs]) ->  groups(Elems) ++ groups(Fs);
groups([?SEQUENCE(Name,_) | Fs])   -> [{Name,1,1} | groups(Fs)];
groups([?CHOICE(_,none,Elems) | Fs]) -> groups(Elems) ++ groups(Fs);
groups([?CHOICE(_,Name,_) | Fs]) -> [{Name,1,1} | groups(Fs)];
groups([F|Fs]) when is_record(F,ystmt) -> [F | groups(Fs)];
groups([]) -> [].

format_record_fields([F]) ->
    format_record_field(F,"");
format_record_fields([F|Fs]) ->
    [format_record_field(F,",") | format_record_fields(Fs)];
format_record_fields([]) ->
    [].

format_record_field(Y=#ystmt{name=Name},Sep) ->
    Default = format_default_value(Y),
    io_lib:format("  ~p = ~s~s\n", [Name,Default,Sep]);
format_record_field({Name,Min,Max}, Sep) when is_atom(Name) ->
    Default = format_default_value({Min,Max}),
    io_lib:format("  ~p = ~s~s\n", [Name,Default,Sep]).

format_default_value(Y) when is_record(Y,ystmt) ->
    format_default_value_({Y#ystmt.min_occure,Y#ystmt.max_occure});
format_default_value(Occure) ->
    format_default_value_(Occure).

format_default_value_({0,n}) -> "[]";
format_default_value_(_) -> "undefined".

%% @doc
%%    Expand argument regular expression
%% @end
expand_arg({const,String}) -> {const,String};
expand_arg({re,String}) -> {re,String};
expand_arg(Name) when is_atom(Name) ->
    Lookup = fun(X) ->
		     RE = arg(X),
		     {true,yang_re:normalize(RE)}
	     end,
    yang_re:expand(Name, Lookup).

%%
%%
%%
test_match(Arg, String) ->
    A0 = yang_validate:expand_arg(Arg),
    A1 = yang_re:format(A0),
    A2 = lists:flatten(["^",A1,"$"]),
    R1 = re:run(String, A2, [{capture,first,index},dotall]),
    {ok,RE} = re:compile(A2, [dotall]),
    R2 = re:run(String, RE, [{capture,first,index}]),
    {R1,R2}.

%%
%% Process statement list and count how many times each
%% statement type occure return a dictionary with all counts
%%
process_statement_list(List) ->
    process_statement_list({undefined,"",0},List,dict:new()).

process_statement_list(Context,[{Stmt,_Ln,Arg,_}|StmtList],Dict) ->
    Key = statement_key(Context,Stmt,Arg),
    process_statement_list(Context, StmtList,
			   dict:update_counter(Key, 1, Dict));
process_statement_list(_Context,[],Dict) ->
    Dict.

%%
%% Simple access function
%%
context_statement({Name,_,_,_}) ->
    Name.

context_key({Name,_,_,_}) ->
    statement_key(undefined,Name,[]).

context_file({_,File,_,_}) ->
    File.

context_line({_,_File,Line,_}) ->
    Line.

context_options({_,_File,_Line,Options}) ->
    Options.

context_option(Key, {_,_File,_Line,Options}) ->
    proplists:get_value(Key, Options).

context_option(Key, {_,_File,_Line,Options}, Default) ->
    proplists:get_value(Key, Options, Default).

%%
%% Statement name, return something that can be formatted with ~s
%%
statement_name(Stmt) when is_atom(Stmt) ->
    Stmt;
statement_name(Stmt) when is_binary(Stmt) ->
    Stmt;
statement_name({Prefix,Stmt}) ->
    <<Prefix/binary, Stmt/binary>>.

%%
%% Transform statment names into context dependent statement names
%% using + as combining char. Also transform unknown statments
%% into '$unknown' to allow simple rule hanlding.
%%
statement_key(Context,{Stmt,_Ln,Arg,_}) ->
    statement_key(context_statement(Context),Stmt,Arg).

statement_key('uses','augment',_) -> 'augment@uses';
statement_key(_, 'deviate','not-supported') -> 'deviate+not-supported';
statement_key(_, 'deviate','add') -> 'deviate+add';
statement_key(_, 'deviate','delete') -> 'deviate+delete';
statement_key(_, 'deviate', 'replace') -> 'deviate+replace';
statement_key(_, Name, _Arg) when is_atom(Name) -> Name;
statement_key(_, Name, _Arg) when is_binary(Name) -> ?UNKNOWN;
%% FIXME: what about this?
statement_key(_, {_Pfx,Name}, _Arg) when is_binary(Name) -> ?UNKNOWN.

%% @doc
%%    Validate YANG stement list input given a parent statement
%% @end
-spec is_valid({Stmt::atom(),File::string(),
		Line::integer(),Options::[{atom(),any()}]},
	       Arg::any(), List::[yang_statement()]) -> boolean().

is_valid({'deviate',File,Line,Opts},'not-supported',List) ->
    is_valid_({'deviate+not-supported',File,Line,Opts},
	      <<"not-supported">>,List);
is_valid({'deviate',File,Line,Opts},'add',List) ->
    is_valid({'deviate+add',File,Line,Opts},
	     <<"add">>, List);
is_valid({'deviate',File,Line,Opts},'delete',List) ->
    is_valid_({'deviate+delete',File,Line,Opts},
	     <<"delete">>, List);
is_valid({'deviate',File,Line,Opts},'replace',List) ->
    is_valid_({'deviate+replace',File,Line,Opts},
	     <<"replace">>, List);
is_valid(Stmt, Arg, List) ->
    is_valid_(Stmt, Arg, List).

%%
%% Process statement list and check stements according to rule
%%
-spec is_valid_(Context::{Stmt::atom(),File::string(),
			  Line::integer(),Options::[{atom(),any()}]},
		Arg::binary(),
		List::[yang_statement()]) -> boolean().

is_valid_(Context,Arg,List) ->
    Cnt = process_statement_list(Context,List,dict:new()),
%%    io:format("~p:\n", [Context]),
%%    dict:fold(fun(Key,Value,_Acc) ->
%%		      io:format("  ~p: count=~w\n", [Key,Value])
%%	      end, ok, Cnt),
    {_RuleName,ArgName,Rule} = rule(context_key(Context)),
    List1 = validate_rule(Context,Rule,List,Cnt),
    List2 = remove_elements(length(List1), ?UNKNOWN, Context, List1),
    %% Check argument
    RR = is_valid_argument(Context,ArgName,Arg),
    if List2 =:= [] ->
	    RR;
       true ->
	    lists:foreach(
	      fun({Stmt,Ln,_Arg,_Stmts}) ->
		      io:format("~s:~w: statement '~s' not valid in context '~s':~w\n",
				[context_file(Context), Ln,
				 statement_name(Stmt),
				 statement_name(context_statement(Context)),
				 context_line(Context)])
	      end, List2),
	    false
    end.

is_valid_argument(Context,ArgName,Arg) ->
    RE = make_re(Context,ArgName),
    BinArg = if is_atom(Arg) ->
		     list_to_binary(atom_to_list(Arg));
		is_binary(Arg) ->
		     Arg;
		Arg =:= [] ->
		     <<>>
	     end,
    case re:run(BinArg, RE, [{capture,first,index}]) of
	nomatch ->
	    io:format("BinArg: ~p\n", [BinArg]),
	    %% io:format("RE: ~s = ~p\n", [ArgName, RE]),
	    io:format("~s:~w: invalid argument to statement ~p, does not have the form ~p\n",
		      [context_file(Context),
		       context_line(Context),
		       context_statement(Context),
		       ArgName]),
	    false;
	{match,_} ->
	    true
    end.

%%
%% Use process dictionary as a regular epression cache
%% Practical but may (as always) bite back.
%%
make_re(Context,ArgName) when is_atom(ArgName) ->
    case get({re,ArgName}) of %% check string
	undefined ->
	    Expanded = expand_arg(ArgName),
	    RE_Expr0 = yang_re:format(Expanded),
	    RE_Expr1 = lists:flatten(["^",RE_Expr0,"$"]),
	    %% put({restr,ArgName}, RE_Expr1),  %% debugging
	    try re:compile(RE_Expr1,[dotall]) of
		{ok,RE} ->
		    %% ugly caching - better variant?
		    put({re,ArgName}, RE),
		    RE;
		{error,Reason} ->
		    io:format("~s:~w: internal error, can not compile expression for ~p, reason: ~p\n",
			      [context_file(Context),
			       context_line(Context),
			       context_statement(Context),
			       Reason]),
		    {ok,RE} = re:compile("^(.*)$",[dotall]),
		    %% make world a better place
		    put({re,ArgName}, RE),
		    RE
	    catch
		error:Reason ->
		    io:format("~s:~w: internal error, can not compile expression for ~p, reason: ~p\n",
			      [context_file(Context),
			       context_line(Context),
			       context_statement(Context),
			       Reason]),
		    {ok,RE} = re:compile("^(.*)$",[dotall]),
		    %% make world a better place
		    put({re,ArgName}, RE),
		    RE
	    end;
	RE ->
	    RE
    end.



validate_rule(Context,R,List,Cnt) ->
    case R of
	[] -> List;
	?SEQUENCE(_,Rule) ->
	    validate_sequence(Context,Rule,List,Cnt);
	?ALL(Min,_,Rule) ->
	    validate_all(Min,Context,Rule,List,List,Cnt);
	?CHOICE(Min,_,Rule) ->
	    validate_choice(Min,Context,Rule,List,Cnt)
    end.

validate_sequence(Context,[R|Rs],List,Cnt) ->
    case R of
	?Y(_Min,_Max,_Name) ->
	    List1 = validate_statement(Context,R,sequence,List,Cnt),
	    validate_sequence(Context,Rs,List1,Cnt);
	?ALL(Min,_Name,Elems) ->
	    List1 = validate_all(Min,Context, Elems,List,List,Cnt),
	    validate_sequence(Context,Rs,List1,Cnt);
	?CHOICE(Min,_Name,Elems) ->
	    List1 = validate_choice(Min,Context,Elems,List,Cnt),
	    validate_sequence(Context,Rs,List1,Cnt);
	?SEQUENCE(_Name,Elems) ->
	    List1 = validate_sequence(Context, Elems, List, Cnt),
	    validate_sequence(Context,Rs,List1,Cnt)
    end;
validate_sequence(Context,[],List,_Cnt) ->
    remove_elements(length(List), ?UNKNOWN, Context, List).


validate_all(Min,Context,[R|Rs],List,List0,Cnt) ->
    case R of
	?Y(_Min,_Max,_Name) ->
	    List1 = validate_statement(Context,R,all,List,Cnt),
	    validate_all(Min,Context,Rs,List1,List0,Cnt);
	?ALL(Min1,_Name,Elems) -> %% check min!
	    List1 = validate_all(Min1,Context,Elems,List,List,Cnt),
	    validate_all(Min,Context,Rs,List1,List0,Cnt);
	?CHOICE(Min1,_Name,Elems) -> %% check min!
	    List1 = validate_choice(Min1,Context, Elems, List, Cnt),
	    validate_all(Min,Context,Rs,List1,List0,Cnt);
	?SEQUENCE(_Name,Elems) ->
	    List1 = validate_sequence(Context, Elems, List, Cnt),
	    validate_all(Min,Context,Rs,List1,List0,Cnt)
    end;
validate_all(Min,Context,[],List,List0,_Cnt) ->
    if Min =:= 0 ->
	    remove_elements(length(List), ?UNKNOWN, Context, List);
       Min =:= 1 ->
	    if List =/= List0 ->
		    remove_elements(length(List), ?UNKNOWN, Context, List);
	       true ->
		    io:format("~s:~w: all not taken\n",
			      [context_file(Context),
			       context_line(Context)]),
		    remove_elements(length(List), ?UNKNOWN, Context, List)
	    end;
       true ->
	    io:format("warning: ALL with min > 1 not supported yet!\n"),
	    remove_elements(length(List), ?UNKNOWN, Context, List)
    end.


validate_choice(Min,Context,[R|Rs],List,Cnt) ->
    case R of
	?Y(_Min,_Max,_Name) ->
	    case validate_statement(Context,R,choice,List,Cnt) of
		List -> validate_choice(Min,Context,Rs,List,Cnt);
		List1 -> List1
	    end;
	?ALL(Min1,_Name,Elems) ->
	    case validate_all(Min1,Context,Elems,List,List,Cnt) of
		List -> validate_choice(Min,Context,Rs,List,Cnt);
		List1 -> List1
	    end;
	?CHOICE(Min1,_Name,Elems) ->
	    case validate_choice(Min1,Context,Elems,List,Cnt) of
		List -> validate_choice(Min,Context,Rs,List,Cnt);
		List1 -> List1
	    end;
	?SEQUENCE(_Name,Elems) ->
	    case validate_sequence(Context, Elems, List, Cnt) of
		List -> validate_choice(Min,Context,Rs,List,Cnt);
		List1 -> List1
	    end
    end;
validate_choice(Min,Context,[],List,_Cnt) ->
    %% pick first element in List to get aprox line number
    if Min =:= 0 ->
	    remove_elements(length(List), ?UNKNOWN, Context, List);
       true ->
	    io:format("~s:~w: choice not taken\n",
		      [context_file(Context),
		       context_line(Context)]),
	    remove_elements(length(List), ?UNKNOWN, Context, List)
    end.

%%
%% Check one statement:
%% - check that the statement occures in quantities specified
%% - if it occurs then check the prefered order against List
%%
validate_statement(Context,?Y(Min,Max,Name),Where,List,Cnt) ->
    N = counter_value(Name, Cnt),
    if is_integer(Min), N < Min ->
	    if Where =:= choice ->
		    remove_elements(N, Name, Context, List);
	       true ->
		    NS = if N =:= 0 -> "";
			    true -> [$\s|integer_to_list(Min-N)]
			 end,
		    io:format("~s:~w: warning missing~s required statement '~s'\n",
			      [context_file(Context),
			       context_line(Context),
			       NS,Name]),
		    remove_elements(N, Name, Context, List)
	    end;
       is_integer(Max), N > Max ->
	    %% FIXME find last statement Name and use line number
	    io:format("~s:~w: warning: too many (~w) statements ~s\n",
		      [context_file(Context),
		       context_line(Context),
		       (N-Max), Name]),
	    remove_elements(N, Name, Context, List);
       N >= Min, N =< Max;  N >= Min, N =:= n ->
	    if Where =:= all ->
		    remove_elements(N, Name, Context, List);
	       true ->
		    case context_option(warn_canonical_order,Context,false) of
			false ->
			    remove_elements(N, Name, Context, List);
			true ->
			    canonical_remove_elements(N, Name,  Context, List)
		    end
	    end
    end.

%%
%% check that statement is the prefered one
%%
canonical_remove_elements(0, _Name, _Context, List) ->
    List;
canonical_remove_elements(_N, _Name, _Context, []) -> %% FIXME
    %% Maybe we should delete counter when elements are removed!
    [];
canonical_remove_elements(N, Name, Context, [{Stmt,Ln,Arg,_}|List]) ->
    Key = statement_key(Context,Stmt,Arg),
    if Key =:= Name ->
	    canonical_remove_elements(N-1, Name, Context, List);
       Key =:= ?UNKNOWN ->
	    canonical_remove_elements(N, Name, Context, List);
       true ->
	    io:format("~s:~w: warning: statement '~s' not in canoical order, for context ~p:~w\n",
		      [context_file(Context), Ln,
		       statement_name(Stmt),
		       context_statement(Context),
		       context_line(Context)
		      ]),
	    remove_elements(N, Name, Context, List)
    end.
%%
%% remove N statments with name Name
%%
remove_elements(0, _Name, _Context, List) ->
    List;
remove_elements(_N, _Name, _Context, []) -> %% FIXME
    %% Maybe we should delete counter when elements are removed!
    [];
remove_elements(N, Name, Context, [E={Stmt,_Ln,Arg,_}|List]) ->
    Key = statement_key(Context,Stmt,Arg),
    if Key =:= Name ->
	    remove_elements(N-1,Name,Context,List);
       true ->
	    [E|remove_elements(N,Name,Context,List)]
    end.


count(Key, Val, Dict) ->
    Dict1 = dict:update_counter(Key, Val, Dict),
    Val1 = dict:fetch(Key, Dict1),
    {Dict,Val1}.

counter_value(Key, Dict) ->
    case dict:find(Key, Dict) of
	error -> 0;
	{ok,Value} -> Value
    end.
%%
%% @doc
%%    List of YANG statments
%% @end
%%
statements() ->
    [
     'anyxml',
     'argument',
     'augment',
     'base',
     'belongs-to',
     'bit',
     'case',
     'choice',
     'config',
     'contact',
     'container',
     'default',
     'description',
     'enum',
     'error-app-tag',
     'error-message',
     'extension',
     'deviation',
     'deviate+not-supported',
     'deviate+add',
     'deviate+delete',
     'deviate+replace',
     'feature',
     'fraction-digits',
     'grouping',
     'identity',
     'if-feature',
     'import',
     'include',
     'input',
     'key',
     'leaf',
     'leaf-list',
     'length',
     'list',
     'mandatory',
     'max-elements',
     'min-elements',
     'module',
     'must',
     'namespace',
     'notification',
     'ordered-by',
     'organization',
     'output',
     'path',
     'pattern',
     'position',
     'prefix',
     'presence',
     'range',
     'reference',
     'refine',
     'require-instance',
     'revision',
     'revision-date',
     'rpc',
     'status',
     'submodule',
     'type',
     'typedef',
     'unique',
     'units',
     'uses',
     'value',
     'when',
     'yang-version',
     'yin-element'].

rule('module') ->
    {'module',identifier,
     ?SEQUENCE(none,[
		     ?ALL(0,none,
			  [
			   ?Y(0,1,'yang-version'),
			   ?Y(1,1,'namespace'),
			   ?Y(1,1,'prefix')
			  ]),

		     ?ALL(0,none,[
				  ?Y(0,n,'import'),
				  ?Y(0,n,'include')
				 ]),

		     ?ALL(0,none,[
				  ?Y(0,1,'organization'),
				  ?Y(0,1,'contact'),
				  ?Y(0,1,'description'),
				  ?Y(0,1,'reference')
				 ]),

		     ?Y(0,n,'revision'),

		     ?ALL(0,body,[
				  ?Y(0,n,'extension'),
				  ?Y(0,n,'feature'),
				  ?Y(0,n,'identity'),
				  ?Y(0,n,'typedef'),
				  ?Y(0,n,'grouping'),
				  ?Y(0,n,'container'),
				  ?Y(0,n,'leaf'),
				  ?Y(0,n,'leaf-list'),
				  ?Y(0,n,'list'),
				  ?Y(0,n,'choice'),
				  ?Y(0,n,'anyxml'),
				  ?Y(0,n,'uses'),
				  ?Y(0,n,'augment'),
				  ?Y(0,n,'rpc'),
				  ?Y(0,n,'notification'),
				  ?Y(0,n,'deviation')
				 ])
		    ])};
rule('submodule') ->
    {'submodule',identifier,
     ?SEQUENCE(none,[
		     ?ALL(0,none,[
				  ?Y(0,1,'yang-version'),
				  ?Y(1,1,'belongs-to')
				 ]),

		     ?ALL(0,none,[
				  ?Y(0,n,'import'),
				  ?Y(0,n,'include')
				 ]),

		     ?ALL(0,none,[
				  ?Y(0,1,'organization'),
				  ?Y(0,1,'contact'),
				  ?Y(0,1,'description'),
				  ?Y(0,1,'reference')
				 ]),

		     ?Y(0,n,'revision'),

		     ?ALL(0,body,[
				  ?Y(0,n,'extension'),
				  ?Y(0,n,'feature'),
				  ?Y(0,n,'identity'),
				  ?Y(0,n,'typedef'),
				  ?Y(0,n,'grouping'),
				  ?Y(0,n,'container'),
				  ?Y(0,n,'leaf'),
				  ?Y(0,n,'leaf-list'),
				  ?Y(0,n,'list'),
				  ?Y(0,n,'choice'),
				  ?Y(0,n,'anyxml'),
				  ?Y(0,n,'uses'),
				  ?Y(0,n,'augment'),
				  ?Y(0,n,'rpc'),
				  ?Y(0,n,'notification'),
				  ?Y(0,n,'deviation')
				  ])
		     ])};
rule('yang-version') ->
    {'yang-version', yang_version, []};
rule('import') ->
    {'import', identifier,
     ?ALL(0,none,[
		  ?Y(1,1,'prefix'),
		  ?Y(0,1,'revision-date')
		 ])};
rule('include') ->
    {'include',identifier,
     ?ALL(0,none,[
		  ?Y(0,1,'revision-date')
		 ])};
rule('namespace') ->
    {'namespace', uri, []};
rule('prefix') ->
    {'prefix', prefix, []};
rule('belongs-to') ->
    {'belongs-to',identifier,
     ?ALL(0,none,[
		  ?Y(1,1,'prefix')
		 ])};
rule('organization') ->
    {'organization', string, []};
rule('contact') ->
    {'contact', string, []};
rule('description') ->
    {'description', string, []};
rule('reference') ->
    {'reference',string,[]};
rule('units') ->
    {'units',string,[]};
rule('revision') ->
    {'revision',date,
     ?ALL(0,none,[
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference')
		 ])};
rule('revision-date') ->
    {'revision-date',date,[]};
rule('extension') ->
    {'extension',identifier,
     ?ALL(0,none,[
		  ?Y(0,1,'argument'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference')
		 ])};
rule('argument') ->
    { 'argument',identifier,
      ?ALL(0,none,[
		   ?Y(0,1,'yin-element')
		  ])};
rule('yin-element') ->
    { 'yin-element', boolean, []};
rule('identity') ->
    {'identity', identifier,
     ?ALL(0,none,[
		  ?Y(0,1,'base'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference')
		 ])};
rule('base') ->
    { 'base',  identifier_ref, []};
rule('feature') ->
    { 'feature',identifier,
      ?ALL(0,none,[
		   ?Y(0,n,'if-feature'),
		   ?Y(0,1,'status'),
		   ?Y(0,1,'description'),
		   ?Y(0,1,'reference')
		  ])};
rule('if-feature') ->
    {'if-feature',identifier_ref,[]};
rule('typedef') ->
    {'typedef',identifier,
     ?ALL(0,none,[
		  ?Y(1,1,'type'),
		  ?Y(0,1,'units'),
		  ?Y(0,1,'default'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference')
		 ])};
rule('type') ->
    {'type',identifier_ref,
     ?ALL(0,none,[
		   ?Y(0,1,'fraction-digits'), %% decimal64
		   ?Y(0,1,'range'),   %% numerical
		   ?Y(0,1,'length'),
		   ?Y(0,n,'pattern'),  %% string
		   ?Y(0,n,'enum'),   %% enum
		   ?Y(0,1,'path'),   %% leafref FIXED: RFC6020 - Errata ID 2949
		   ?Y(0,1,'base'),   %% identityref
		   ?Y(0,1,'require-instance'),  %% instance-identifier
		   ?Y(0,n,'bit'),               %% bits
		   ?Y(0,n,'type')               %% union
		  ])};
    %% How I would like to have it
    %% {'type',identifier_ref,
    %%  ?CHOICE(0,none‚,[
    %% 		      ?ALL(0,none,[
    %% 				   ?Y(1,1,'fraction-digits'), %% decimal64
    %% 				   ?Y(0,1,range)   %% numerical
    %% 				  ]),
    %% 		      ?Y(1,1,range),  %% numerical
    %% 		      ?ALL(0,none,[
    %% 				   ?Y(0,1,'length'),
    %% 				   ?Y(0,n,'pattern')]), %% string
    %% 		      ?Y(1,n,'enum'),   %% enum
    %% 		      ?Y(1,1,'path'),   %% leafref FIXED: RFC6020 - Errata ID 2949
    %% 		      ?Y(1,1,'base'),   %% identityref
    %% 		      ?Y(0,1,'require-instance'),  %% instance-identifier
    %% 		      ?Y(1,n,'bit'),               %% bits
    %% 		      ?Y(1,n,'type')               %% union
    %% 		     ])};
rule('range') ->
    {'range',range_arg,
     ?ALL(0,none,[
		  ?Y(0,1,'error-message'),
		  ?Y(0,1,'error-app-tag'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference')
		 ])};
rule('fraction-digits') ->
    {'fraction-digits',fraction_digits_arg,[]};
rule('length') ->
    {'length',length_arg,
     ?ALL(0,none,[
		  ?Y(0,1,'error-message'),
		  ?Y(0,1,'error-app-tag'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference')
		  ])};
rule('pattern') ->
    {'pattern',string,
     ?ALL(0,none,[
		  ?Y(0,1,'error-message'),
		  ?Y(0,1,'error-app-tag'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference')
		 ])};
rule('default') ->
    {'default',string,[]};
rule('enum') ->
    {'enum',string,
     ?ALL(0,none,[
		  ?Y(0,1,'value'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference')
		 ])};
rule('path') ->
    {'path',path_arg,[]};
rule('require-instance') ->
    {'require-instance',boolean,[]};
rule('bit') ->
    {'bit',identifier,
     ?ALL(0,none,[
		  ?Y(0,1,'position'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference')
		 ])};
rule('position') ->
    {'position',position_value_arg,[]};
rule('status') ->
    {'status',status_arg,[]};
rule('config') ->
    {'config',boolean,[]};
rule('mandatory') ->
    {'mandatory',boolean,[]};
rule('presence') ->
    {'presence',string,[]};
rule('ordered-by') ->
    {'ordered-by',ordered_by_arg,[]};
rule('must') ->
    { 'must',string,
      ?ALL(0,none,[
		   ?Y(0,1,'error-message'),
		   ?Y(0,1,'error-app-tag'),
		   ?Y(0,1,'description'),
		   ?Y(0,1,'reference')
		   ])};
rule('error-message') ->
    {'error-message',string,[]};
rule('error-app-tag') ->
    {'error-app-tag',string,[]};
rule('min-elements') ->
    {'min-elements',min_value,[]};
rule('max-elements') ->
    {'max-elements',max_value,[]};
rule('value') ->
    {'value',integer_value,[]};
rule('grouping') ->
    {'grouping',identifier,
     ?ALL(0,none,[
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference'),
		  ?ALL(0,definitions,[
				      ?Y(0,n,'typedef'),
				      ?Y(0,n,'grouping')
				     ]),
		  ?ALL(0,data,[
			       ?Y(0,n,'container'),
			       ?Y(0,n,'leaf'),
			       ?Y(0,n,'leaf-list'),
			       ?Y(0,n,'list'),
			       ?Y(0,n,'choice'),
			       ?Y(0,n,'anyxml'),
			       ?Y(0,n,'uses')
			       ])
		  ])};
rule('container') ->
    {'container',identifier,
     ?ALL(0,none,[
		  ?Y(0,1,'when'),
		  ?Y(0,n,'if-feature'),
		  ?Y(0,n,'must'),
		  ?Y(0,1,'presence'),
		  ?Y(0,1,'config'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference'),
		  ?ALL(0,definitions,[
				      ?Y(0,n,'typedef'),
				      ?Y(0,n,'grouping')
				     ]),
		  ?ALL(0,data,[
			       ?Y(0,n,'container'),
			       ?Y(0,n,'leaf'),
			       ?Y(0,n,'leaf-list'),
			       ?Y(0,n,'list'),
			       ?Y(0,n,'choice'),
			       ?Y(0,n,'anyxml'),
			       ?Y(0,n,'uses')
			      ])
		  ])};
rule('leaf') ->
    {'leaf',identifier,
     ?ALL(0,none,[
		  ?Y(0,1,'when'),
		  ?Y(0,n,'if-feature'),
		  ?Y(1,1,'type'),
		  ?Y(0,1,'units'),
		  ?Y(0,n,'must'),
		  ?Y(0,1,'default'),
		  ?Y(0,1,'config'),
		  ?Y(0,1,'mandatory'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference')
		 ])};
rule('leaf-list') ->
    {'leaf-list', identifier,
     ?ALL(0,none,[
		  ?Y(0,1,'when'),
		  ?Y(0,n,'if-feature'),
		  ?Y(1,1,'type'),
		  ?Y(0,1,'units'),
		  ?Y(0,n,'must'),
		  ?Y(0,1,'config'),
		  ?Y(0,1,'min-elements'),
		  ?Y(0,1,'max-elements'),
		  ?Y(0,1,'ordered-by'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference')
		 ])};
rule('list') ->
    {'list',identifier,
     ?ALL(0,none,[
		  ?Y(0,1,'when'),
		  ?Y(0,n,'if-feature'),
		  ?Y(0,n,'must'),
		  ?Y(0,1,'key'),
		  ?Y(0,n,'unique'),
		  ?Y(0,1,'config'),
		  ?Y(0,1,'min-elements'),
		  ?Y(0,1,'max-elements'),
		  ?Y(0,1,'ordered-by'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference'),
		  ?ALL(0,definitions,[
				      ?Y(0,n,'typedef'),
				      ?Y(0,n,'grouping')
				     ]),
		  ?ALL(1,data,[
			       ?Y(0,n,'container'),
			       ?Y(0,n,'leaf'),
			       ?Y(0,n,'leaf-list'),
			       ?Y(0,n,'list'),
			       ?Y(0,n,'choice'),
			       ?Y(0,n,'anyxml'),
			       ?Y(0,n,'uses')
			      ])
		  ])};
rule('key') ->
    {'key',key_arg,[]};
rule('unique') ->
    {'unique',unique_arg,[]};
rule('choice') ->
    {'choice',identifier,
     ?ALL(0,none,[
		  ?Y(0,1,'when'),
		  ?Y(0,n,'if-feature'),
		  ?Y(0,1,'default'),
		  ?Y(0,1,'config'),
		  ?Y(0,1,'mandatory'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference'),
		  ?ALL(0,data,[
			       ?Y(0,n,'container'),
			       ?Y(0,n,'leaf'),
			       ?Y(0,n,'leaf-list'),
			       ?Y(0,n,'list'),
			       ?Y(0,n,'anyxml'),
			       ?Y(0,n,'case')
			      ])
		 ])};
rule('case') ->
    {'case',identifier,
     ?ALL(0,none,[
		  ?Y(0,1,'when'),
		  ?Y(0,n,'if-feature'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference'),
		  ?ALL(0,data,[
			       ?Y(0,n,'container'),
			       ?Y(0,n,'leaf'),
			       ?Y(0,n,'leaf-list'),
			       ?Y(0,n,'list'),
			       ?Y(0,n,'choice'),
			       ?Y(0,n,'anyxml'),
			       ?Y(0,n,'uses')
			      ])
		 ])};
rule('anyxml') ->
    {'anyxml',identifier,
     ?ALL(0,none,[
		  ?Y(0,1,'when'),
		  ?Y(0,n,'if-feature'),
		  ?Y(0,n,'must'),
		  ?Y(0,1,'config'),
		  ?Y(0,1,'mandatory'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference')
		 ])};
rule('uses') ->
    {'uses',identifier_ref,
     ?ALL(0,none,[
		  ?Y(0,1,'when'),
		  ?Y(0,n,'if-feature'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference'),
		  ?Y(0,n,'refine'),
		  ?Y(0,n,'augment@uses')
		 ])};
rule('refine') ->
    {'refine', refine_arg,
     ?ALL(0,none,[  %% must validate according to refined node later
		    ?Y(0,n,'must'),
		    ?Y(0,1,'default'),
		    ?Y(0,1,'presence'),
		    ?Y(0,1,'config'),
		    ?Y(0,1,'min-elements'),
		    ?Y(0,1,'max-elements'),
		    ?Y(0,1,'mandatory'),
		    ?Y(0,1,'description'),
		    ?Y(0,1,'reference')
		 ])};
rule('augment@uses') -> %% SPECIAL:  'augment' inside 'uses' statement
    {'augment',uses_augment_arg,
     ?ALL(0,none,[
		  ?Y(0,1,'when'),
		  ?Y(0,n,'if-feature'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference'),
		  ?ALL(1,data,[
			       ?Y(0,n,'container'),
			       ?Y(0,n,'leaf'),
			       ?Y(0,n,'leaf-list'),
			       ?Y(0,n,'list'),
			       ?Y(0,n,'choice'),
			       ?Y(0,n,'anyxml'),
			       ?Y(0,n,'uses'),
			       ?Y(0,n,'case')])
		 ])};
rule('augment') ->
    {'augment',augment_arg,
     ?ALL(0,none,[
		  ?Y(0,1,'when'),
		  ?Y(0,n,'if-feature'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference'),
		  ?ALL(1,data,[
			       ?Y(0,n,'container'),
			       ?Y(0,n,'leaf'),
			       ?Y(0,n,'leaf-list'),
			       ?Y(0,n,'list'),
			       ?Y(0,n,'choice'),
			       ?Y(0,n,'anyxml'),
			       ?Y(0,n,'uses'),
			       ?Y(0,n,'case')])
		 ])};
rule('when') ->
    {'when',string,
     ?ALL(0,none,[
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference')
		 ])};
rule('rpc') ->
    {'rpc',identifier,
     ?ALL(0,none,[
		  ?Y(0,n,'if-feature'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference'),
		  ?ALL(0,definitions,[
				      ?Y(0,n,'typedef'),
				      ?Y(0,n,'grouping')]),
		  ?Y(0,1,'input'),
		  ?Y(0,1,'output')
		 ])};
rule('input') ->
    {'input', empty,
     ?ALL(0,none,[
		  ?ALL(0,definitions,[
				      ?Y(0,n,'typedef'),
				      ?Y(0,n,'grouping')]),
		  ?ALL(1,data,[
			       ?Y(0,n,'container'),
			       ?Y(0,n,'leaf'),
			       ?Y(0,n,'leaf-list'),
			       ?Y(0,n,'list'),
			       ?Y(0,n,'choice'),
			       ?Y(0,n,'anyxml'),
			       ?Y(0,n,'uses')])
		 ])};
rule('output') ->
    {'output', empty,
     ?ALL(0,none,[
		  ?ALL(0,definitions,[
				      ?Y(0,n,'typedef'),
				      ?Y(0,n,'grouping')]),
		  ?ALL(1,data,[
			       ?Y(0,n,'container'),
			       ?Y(0,n,'leaf'),
			       ?Y(0,n,'leaf-list'),
			       ?Y(0,n,'list'),
			       ?Y(0,n,'choice'),
			       ?Y(0,n,'anyxml'),
			       ?Y(0,n,'uses')])
		 ])};
rule('notification') ->
    {'notification',identifier,
     ?ALL(0,none,[
		  ?Y(0,n,'if-feature'),
		  ?Y(0,1,'status'),
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference'),
		  ?ALL(0,definitions,[
				      ?Y(0,n,'typedef'),
				      ?Y(0,n,'grouping')]),
		  ?ALL(0,data,[
			       ?Y(0,n,'container'),
			       ?Y(0,n,'leaf'),
			       ?Y(0,n,'leaf-list'),
			       ?Y(0,n,'list'),
			       ?Y(0,n,'choice'),
			       ?Y(0,n,'anyxml'),
			       ?Y(0,n,'uses')])
		 ])};
rule('deviation') ->
    {'deviation', absolute_schema_nodeid,
     ?ALL(0,none,[
		  ?Y(0,1,'description'),
		  ?Y(0,1,'reference'),
		  ?CHOICE(1,data,[
				  ?Y(1,1,'deviate+not-supported'),
				  ?ALL(1,none,[
					       ?Y(1,n,'deviate+add'),
					       ?Y(1,n,'deviate+replace'),
					       ?Y(1,n,'deviate+delete')])
				 ])
		 ])};
rule('deviate+not-supported') -> %% SPECIAL-1
    {'deviate',{const,"not-supported"},[]};
rule('deviate+add') ->  %% SPECIAL-2
    {'deviate',{const,"add"},
     ?ALL(0,none,[
		  ?Y(0,1,'units'),
		  ?Y(0,n,'must'),
		  ?Y(0,n,'unique'),
		  ?Y(0,1,'default'),
		  ?Y(0,1,'config'),
		  ?Y(0,1,'mandatory'),
		  ?Y(0,1,'min-elements'),
		  ?Y(0,1,'max-elements')
		 ])};
rule('deviate+delete') -> %% SPECIAL-3
    {'deviate',{const,"delete"},
     ?ALL(0,none,[
		  ?Y(0,1,'units'),
		  ?Y(0,n,'must'),
		  ?Y(0,n,'unique'),
		  ?Y(0,1,'default')
		 ])};
rule('deviate+replace') -> %% SPECIAL-4
    {'deviate',{const,"replace"},
     ?ALL(0,none,[
		  ?Y(0,1,'type'),
		  ?Y(0,1,'units'),
		  ?Y(0,1,'default'),
		  ?Y(0,1,'config'),
		  ?Y(0,1,'mandatory'),
		  ?Y(0,1,'min-elements'),
		  ?Y(0,1,'max-elements')
		 ])};
rule(?UNKNOWN) ->  %% translated unknown-statement
    {?UNKNOWN,unknown_arg,
     ?ALL(0,none,[
		  ?Y(0,n,?UNKNOWN)
		 ])};
rule(Stmt) when is_binary(Stmt) ->  %% unknown-statment
    {?UNKNOWN,unknown_arg,
     ?ALL(0,none,[
		  ?Y(0,n,?UNKNOWN)
		 ])}.

%%
%% arg matching
%%
arg(yang_version) -> {const,"1"};
arg(empty)        -> {const,""};
arg(prefix) ->       identifier;
arg(current_function_invocation) ->
    {',',[{const,"current"},optwsp,{const,"\\("},optwsp,{const,"\\)"}]};
arg(string) ->
    {re, ".*"};
arg(identifier) ->
    {re,"(?!(?i)xml)[A-Za-z_](\\w|-|\\.)*"};
arg(identifier_ref) ->
    {',',[{'?',{',',[prefix,{const,":"}]}}, identifier]};
arg(range_arg) ->
    {',',[range_part,
	       {'*',{',',
			 [optsep,{const,"\\|"},
			  optsep,range_part]}}]};
arg(range_part) ->
    {',',[range_boundary,
	       {'?',{',',[optsep,{const,"\\.\\."},
				    optsep, range_boundary]}}]};
arg(range_boundary) ->
    {'|',[{const,"min"},{const,"max"},integer_value,decimal_value]};

arg(length_arg) ->
    {',',[length_part,
	  {'*',{',',
		[optsep,{const,"\\|"},optsep,length_part]}}]};
arg(length_part) ->
    {',',[length_boundary,
	  {'?',{',',
		[optsep,{const,"\\.\\."},optsep,length_boundary]}}]};
arg(length_boundary) ->
    {'|',[{const,"min"},{const,"max"},non_negative_integer_value]};

arg(date) ->
    {re,"\\d{4}-\\d{2}-\\d{2}"};
arg(schema_nodeid) ->
    {'|',[absolute_schema_nodeid, descendant_schema_nodeid]};
arg(absolute_schema_nodeid) ->
    {'+',{',',[{re,"/"},node_identifier]}};
arg(descendant_schema_nodeid) ->
    {',',[node_identifier,{'?',absolute_schema_nodeid}]};
arg(node_identifier) ->
    {',',[{'?',{',',[prefix,{re,":"}]}},identifier]};
arg(instance_identifier) ->
    {'+',{',',[{re,"/"},node_identifier,{'*',predicate}]}};
arg(predicate) ->
    {',',[{const,"\\["},optwsp,
	  {'|',[predicate_expr,pos]},
	  optwsp,{const,"\\]"}]};
arg(predicate_expr) ->
    {',',[{'|',[node_identifier,{const,"\\."}]},
	  optwsp,{const,"="},optwsp,
	  {'|',[{',',[dquote,string,dquote]},
		{',',[squote,string,squote]}]}]};
arg(pos) -> non_negative_integer_value;
arg(path_arg) ->
    {'|',[absolute_path,relative_path]};
arg(absolute_path) ->
    {'+',{',',
	  [{const,"/"},node_identifier,{'*',path_predicate}]}};
arg(relative_path) ->
    {',',[{'+',{',',[{const,"\\.\\."},{const,"/"}]}},
	  descendant_path]};
arg(descendant_path) ->
    {',',[node_identifier,
	  {'?',{',',
		[{'*',path_predicate},absolute_path]}}]};
arg(path_predicate) ->
    {',',[{const,"\\["},optwsp,path_equality_expr,optwsp,{const,"\\]"}]};
arg(path_equality_expr) ->
    {',',[node_identifier,optwsp,{const,"="},optwsp,path_key_expr]};
arg(path_key_expr) ->
    {',',[current_function_invocation,
	  optwsp,{const,"/"},optwsp,rel_path_keyexpr]};
arg(rel_path_keyexpr) ->
    {',',[{'+',
	   {',',[{const,"\\.\\."},optwsp,{const,"/"},optwsp]}},
	  {'+',
	   {',',[node_identifier,
		 {'*',wsp},{const,"/"},optwsp]}},
	  node_identifier]};
arg(fraction_digits_arg) ->  {re,"(1[0-8]?)|[2-9]"};
arg(position_value_arg) ->
    non_negative_integer_value;
arg(status_arg) ->
    {re,"current|obsolete|deprecated"};
arg(ordered_by_arg) ->
    {re,"user|system"};
arg(min_value) ->
    non_negative_integer_value;
arg(max_value) ->
    {'|',[{const,"unbounded"},non_negative_integer_value]};
arg(key_arg) ->
    {',',[node_identifier,{'*',{',',[sep,node_identifier]}}]};
arg(unique_arg) ->
    {',',[descendant_schema_nodeid,{'*',{',',[sep,descendant_schema_nodeid]}}]};
arg(refine_arg) ->          descendant_schema_nodeid;
arg(uses_augment_arg) ->    descendant_schema_nodeid;
arg(augment_arg) ->         absolute_schema_nodeid;
arg(boolean) -> {re,"(true|false)"};
arg(integer_value) ->
    {'|',[{',',[{const,"-"},non_negative_integer_value]},
	     non_negative_integer_value]};
arg(decimal_value) ->
    {',',[integer_value,{const,"\\."},zero_integer_value]};
arg(non_negative_integer_value) ->
    {'|',[{const,"0"},positive_integer_value]};
arg(positive_integer_value) -> {',',[{re,"[1-9]"},{re,"\\d*"}]};
arg(zero_integer_value)     -> {re,"\\d+"};
arg(unknown_arg) -> {re, ".*"};
arg(sep)    -> {'+',space};
arg(optsep) -> {'*',space};
arg(optwsp) -> {'*',wsp};
arg(space)  -> {re, "\\s"};
arg(wsp)    -> {re,"[ \\t]"};
arg(dquote) -> {const,"\""};
arg(squote) -> {const,"'"};
arg(uri) ->
    {',',[scheme,{const,":"},hier_part,
	  {'?',{',',[{const,"\\?"},'query']}},
	  {'?',{',',[{const,"#"},fragment]}}]};
arg(hier_part) ->
    {'|',[{',',[{const,"//"},authority,path_abempty]},
	  path_absolute,
	  path_rootless,
	  path_empty]};
arg(uri_reference) ->
    {'|',[uri,relative_ref]};
arg(absolute_uri) ->
    {',',[scheme,{const,":"},hier_part,
	  {'?',{',',[{const,"\\?"},'query']}}]};
arg(relative_ref) ->
    {',',[relative_part,
	  {'?',{',',[{const,"\\?"},'query']}},
	  {'?',{',',[{const,"#"},fragment]}}]};
arg(relative_part) ->
    {'|',[{',',[{const,"//"},authority,path_abempty]},
	  path_absolute,
	  path_noscheme,
	  path_empty]};
arg(scheme) ->
    {re,"[A-Za-z][A-Za-z0-9+-.]*"};
arg(authority) ->
    {',',[{'?',{',',[userinfo,{const,"@"}]}},
	  host,
	  {'?',{',',[{const,":"},port]}}]};
arg(userinfo) ->
    {'*',{'|',[unreserved,pct_encoded,sub_delims,{const,":"}]}};
arg(host) ->
    {'|',[ip_literal,ipv4address,reg_name]};
arg(port) ->
    {re,"\d*"};
arg(ip_literal) ->
    {',',[{const,"\\["},{'|',[ipv6address,ipvfuture]},{const,"\\]"}]};
arg(ipvfuture) ->
    {',',[{const,"v"},{re,"[0-9A-Fa-f]+"},{re,"[.]"},
	  {'+',{'|',[unreserved,sub_delims,{const,":"}]}}]};
arg(ipv6address) ->
    {'|',
     [{',',[                       {{',',[h16,{const,":"}]},{6,6}},ls32]},
      {',',[          {const,"::"},{{',',[h16,{const,":"}]},{5,5}},ls32]},
      {',',[{'?',h16},{const,"::"},{{',',[h16,{const,":"}]},{4,4}},ls32]},
      {',',[{'?',{',',[{{',',[h16,{const,":"}]},{0,1}},h16]}},
	    {const,"::"},{{',',[h16,{const,":"}]},{3,3}},ls32]},
      {',',[{'?',{',',[{{',',[h16,{const,":"}]},{0,2}},h16]}},
	    {const,"::"},{{',',[h16,{const,":"}]},{2,2}},ls32]},
      {',',[{'?',{',',[{{',',[h16,{const,":"}]},{0,3}},h16]}},
	    {const,"::"},{{',',[h16,{const,":"}]},{1,1}},ls32]},
      {',',[{'?',{',',[{{',',[h16,{const,":"}]},{0,4}},h16]}},
	    {const,"::"},ls32]},
      {',',[{'?',{',',[{{',',[h16,{const,":"}]},{0,5}},h16]}},
	    {const,"::"},h16]},
      {',',[{'?',{',',[{{',',[h16,{const,":"}]},{0,6}},h16]}},
	    {const,"::"}]}]};
arg(hexdig) -> {re,"[0-9A-Fa-f]"};
arg(digit) -> {re,"[0-9]"};
arg(alpha) -> {re,"[A-Za-z]"};
arg(h16)   -> {hexdig,{1,4}};
arg(ls32) ->
    {'|',[{',',[h16,{const,":"},h16]},ipv4address]};
arg(ipv4address) ->
    {',',[dec_octet,{const,"\\."},dec_octet,{const,"\\."},
	  dec_octet,{const,"\\."},dec_octet]};
arg(dec_octet) ->
    {'|',[{re,"[0-9]"},
	  {re,"[1-9][0-9]"},
	  {re,"1[0-9][0-9]"},
	  {re,"2[0-4][0-9]"},
	  {re,"25[0-5]"}]};
arg(reg_name) ->
    {'*',{'|',[unreserved,pct_encoded,sub_delims]}};
arg(path) ->
    {'|',[path_abempty,
	  path_absolute,
	  path_noscheme,
	  path_rootless,
	  path_empty]};
arg(path_abempty) ->
    {'*',{',',[{const,"/"},segment]}};
arg(path_absolute) ->
    {',',[{const,"/"},
	  {'?',{',',[segment_nz,{'*',{',',[{const,"/"},segment]}}]}}]};
arg(path_noscheme) ->
    {',',[segment_nz_nc,{'*',{',',[{const,"/"},segment]}}]};
arg(path_rootless) ->
    {',',[segment_nz,{'*',{',',[{const,"/"},segment]}}]};
arg(path_empty) -> {re,""};
arg(segment) -> {'*',pchar};
arg(segment_nz) -> {'+',pchar};
arg(segment_nz_nc) ->
    {'+', {'|',[unreserved,pct_encoded,sub_delims,{const,"@"}]}};
arg(pchar) ->
    {'|',[unreserved,pct_encoded,sub_delims,{const,":"},{const,"@"}]};
arg('query') ->
    {'*',{'|',[pchar,{const,"/"},{const,"\\?"}]}};
arg(fragment) ->
    {'*',{'|',[pchar,{const,"/"},{const,"\\?"}]}};
arg(pct_encoded) ->
    {',',[{const,"\\%"},hexdig,hexdig]};
arg(unreserved) ->
    {'|',[alpha,digit,{const,"-"},{const,"\\."},
	  {const,"_"},{const,"~"}]};
arg(reserved) ->
    {'|',[gen_delims,sub_delims]};
arg(gen_delims) ->
    {'|',[{const,":"}, {const,"/"}, {const,"\\?"},  {const,"#"},
	  {const,"\\["}, {const, "\\]"}, {const, "@"}]};
arg(sub_delims) ->
    {'|',[{const,"\\!"}, {const,"\\$"}, {const,"\\&"},
	  {const,"'"},   {const,"\\("}, {const, "\\)"},
	  {const,"\\*"}, {const,"\\+"}, {const,","},
	  {const,";"},{const,"="}]};
arg(not_used) ->
    ok.
