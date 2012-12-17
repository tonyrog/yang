-module(yang_codegen).

-export([module/3]).

-include_lib("parse_trans/include/codegen.hrl").

module(YangF, ErlMod, Opts) ->
    case file:read_file(YangF) of
	{ok, Bin} ->
	    YangOpts = proplists:get_value(yang, Opts, []),
	    case yang_parser:deep_parse(YangF, YangOpts) of
		{ok, YangForms} ->
		    module_(YangForms, Bin, ErlMod,
			    lists:keydelete(yang, 1, Opts));
		Error ->
		    Error
	    end;
	Error1 ->
	    Error1
    end.

module_([{module,_,M,Y}] = Yang, Src, ErlMod, Opts) ->
    Forms = [{attribute,1,module,ErlMod},
	     {attribute,2,export,[{module,0},
				  {prefix,0},
				  {revisions,0},
				  {contact,0},
				  {typedef,1},
				  {groupings, 0},
				  {grouping,1},
				  {rpcs, 0},
				  {rpc, 1},
				  {yang, 0},
				  {src, 0}]},
	     module(M),
	     prefix(Y),
	     revisions(Y),
	     contact(Y),
	     typedefs(Y),
	     groupings(Y),
	     grouping_1(Y),
	     rpcs(Y),
	     rpc_1(Y),
	     yang(Yang),
	     src(Src)],
    case compile:forms(Forms, Opts) of
	{ok, ModName, Bin} ->
	    io:fwrite("loading binary (~p)~n", [ModName]),
	    code:load_binary(
	      ModName, "/tmp/" ++ atom_to_list(ModName) ++ ".beam", Bin);
	Other ->
	    Other
    end.

module(M) ->
    codegen:gen_function(
      module,
      fun() ->
	      {'$var', M}
      end).

prefix(Y) ->
    Prefix = lists:keyfind(prefix, 1, Y),
    codegen:gen_function(
      prefix,
      fun() ->
	      {'$var', Prefix}
      end).

revisions(Y) ->
    Revs = [R || {revision,_,_,_} = R <- Y],
    codegen:gen_function(
      revisions,
      fun() ->
	      {'$var', Revs}
      end).

contact(Y) ->
    Contact = lists:keyfind(contact, 1, Y),
    codegen:gen_function(
      contact,
      fun() ->
	      {'$var', Contact}
      end).

typedefs(_Y) ->
    codegen:gen_function(
      typedef,
      fun(_) ->
	      []
      end).

groupings(Y) ->
    Names = [Name || {grouping, _, Name, _} <- Y],
    codegen:gen_function(
      groupings,
      fun() ->
	      {'$var', Names}
      end).

grouping_1(Y) ->
    codegen:gen_function(
      grouping,
      [fun({'$var', Name}) ->
	       {'$var', Grp}
       end || {grouping,_,Name,_} = Grp <- Y]).

rpcs(Y) ->
    Names = [Name || {rpc,_,Name,_} <- Y],
    codegen:gen_function(
      rpcs,
      fun() ->
	      {'$var', Names}
      end).

rpc_1(Y) ->
    codegen:gen_function(
      rpc,
      [fun({'$var', Name}) ->
	       {'$var', RPC}
       end || {rpc,_,Name,_} = RPC <- Y]).

yang(_Y) ->
    codegen:gen_function(
      yang,
      fun() ->
	      %% {'$var', Y}
	      []
      end).

src(_Bin) ->
    codegen:gen_function(
      src,
      fun() ->
	      %% {'$var', Bin}
	      []
      end).
