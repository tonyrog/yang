% Copyright (c) 2010-2011 by Travelping GmbH <info@travelping.com>

% Permission is hereby granted, free of charge, to any person obtaining a
% copy of this software and associated documentation files (the "Software"),
% to deal in the Software without restriction, including without limitation
% the rights to use, copy, modify, merge, publish, distribute, sublicense,
% and/or sell copies of the Software, and to permit persons to whom the
% Software is furnished to do so, subject to the following conditions:

% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.

% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
% DEALINGS IN THE SOFTWARE.

-module(yang_typespec).

-export([typespec/1, pretty_print/1, pretty_print/2, pretty_print_rpc/1]).
-export([get_type/2, hrl/1, hrl/2]).
-export([rpc_methods/1, rpc_params/2]).

-include("typespec.hrl").

%% --------------------------------------------------------------------------------
%% -- API functions
typespec([{module,_,M,Data}]) ->
    {Fields, NOpts} = element_mapfold(fun yang_typespec/1, Data),
    io:format("unhandeld opts: ~p~n", [NOpts]),
    {M, Fields}.

rpc_methods({_Module, TypeSpec}) ->
    [Name || #rpc{name = Name} <- TypeSpec].

rpc_params(RPC, Ms = {_Module, TypeSpec}) ->
    case lists:keyfind(RPC, #rpc.name, TypeSpec) of
	#rpc{input = Input} ->
	    [element(2, X) || X <- Input#object.fields];
	_ ->
	    error({badarg, [rpc_params, RPC, Ms]})
    end.

get_type({_Module, TypeSpec}, Type)
  when is_list(Type) ->
    lists:foldl(fun(_, false) -> false;
		   (T, Acc)  -> typefind(T, Acc) end,
		TypeSpec, Type);
get_type({_Module, TypeSpec}, Type) ->
    typefind(Type, TypeSpec).

hrl(Path, Ms = {Module, _}) ->
    F = << (filename:join(Path, Module))/binary, <<".hrl">>/binary >>,
    file:write_file(F, hrl(Ms)).

hrl(Ms) ->
    io_lib:format("typespec() -> ~n~s.~n", [pretty_print(Ms)]).

pretty_print(ModuleSpec) ->
    io_lib_pretty:print(ModuleSpec, fun record_definition/2).

pretty_print({Module, TypeSpec}, Filter) ->
    io_lib_pretty:print({Module, Filter(TypeSpec)}, fun record_definition/2).

pretty_print_rpc(ModuleSpec) ->
    pretty_print(ModuleSpec, fun(T) -> [X || X = #rpc{} <- T] end).

%% --------------------------------------------------------------------------------
%% -- internal functions

-define(O2R(Init, Record, InOpts), opts_to_record(2, record_info(fields, Record), InOpts, Init)).
-define(RD(Record), record_definition(Record, _) -> record_info(fields, Record)).

?RD(field);
?RD(typedef);
?RD(struct);
?RD(object);
?RD(array);
?RD(rpc);
?RD(choice);
?RD('case');
record_definition(_, _) -> no.


rpcfind(input, #rpc{input = Input}) ->
    Input;
rpcfind(output, #rpc{input = Output}) ->
    Output;
rpcfind(Field, #rpc{fields = Fields}) ->
    typefind(Field, Fields).

typefind(Name, #field{type = Type})    -> typefind(Name, Type);
typefind(Name, #typedef{type = Type})  -> typefind(Name, Type);
typefind(Name, #struct{fields = Type}) -> typefind(Name, Type);
typefind(Name, #object{fields = Type}) -> typefind(Name, Type);
typefind(Name, #array{type = Type})    -> typefind(Name, Type);
typefind(Name, #rpc{fields = Type})    -> typefind(Name, Type);
typefind(Name, #choice{cases = Type})  -> typefind(Name, Type);
typefind(Name, #'case'{fields = Type}) -> typefind(Name, Type);
typefind({rpc, Name, InOut}, TypeSpec)
  when is_list(TypeSpec) ->
    case lists:keyfind(Name, 2, TypeSpec) of
	RPC when is_record(RPC, rpc) ->
	    rpcfind(InOut, RPC);
	_ ->
	    throw({error, unknown_method})
    end;
typefind(Name, TypeSpec)
  when is_list(TypeSpec) ->
    lists:keyfind(Name, 2, TypeSpec).

yang_typespec({rpc,_,Name, Opts}) ->
    rpc(Name, Opts);
yang_typespec(Elem) ->
    element(Elem).

opts_get_value(Key, List) ->
    opts_get_value(Key, List, undefined).
opts_get_value(Key, List, Default) ->
    case lists:keyfind(Key, 1, List) of
	{Key,_,Value,_} -> Value;
	_               -> Default
    end.

%% anyxml

%% config
%% if-feature
%% must
%% reference
%% status
%% uses
%% when

element({leaf,_,Name,Opts}) ->
    field(Name, Opts);
element({'leaf-list',_,Name,Opts}) ->
    ?O2R(#array{name = Name}, array, Opts);
element({list,_,Name,Opts}) ->
    S = ?O2R(#array{name = Name}, array, Opts),
    {Fields, NOpts} = elements(S#array.opts),
    S#array{type = #struct{fields = Fields, opts = []}, opts = NOpts};
element({container,_,Name,Opts}) ->
    object(Name, Opts);
element({grouping,_,Name,Opts}) ->
    object(Name, Opts);
element({typedef,_,Name,Opts}) ->
    ?O2R(#typedef{name = Name}, typedef, Opts);
element({choice,_,Name,Opts}) ->
    R = ?O2R(#choice{name = Name}, choice, Opts),
    {Fields, NOpts} = element_mapfold(fun cases/1, R#choice.opts),
    R#choice{cases = Fields, opts = NOpts};
element({prefix, _, Name, _}) ->
    put(prefix, Name),
    [];
element(_) ->
    [].

cases({'case',_,Name,Opts}) ->
    R = ?O2R(#'case'{name = Name}, 'case', Opts),
    {Fields, NOpts} = elements(R#'case'.opts),
    R#'case'{fields = Fields, opts = NOpts};
cases(E) ->
    element(E).

elements(Opts) ->
    element_mapfold(fun element/1, Opts).

element_mapfold(Fun, Opts) ->
    {RemOpts, Fields} = lists:mapfoldl(
        fun(Elem, Fields) ->
                case Fun(Elem) of
                    []    ->
                        {Elem, Fields};
                    NElem ->
                        {[], [NElem|Fields]}
                end
        end, [], Opts),
    {lists:reverse(Fields), lists:flatten(RemOpts)}.

type(<<"enumeration">>, Opts) ->
    Enum = [X || {enum,_,X,_} <- Opts],
    #enumeration{enum = Enum};
type(<<"string">>, Opts) ->
    ?O2R(#string{}, string, Opts);
type(<<"binary">>, Opts) ->
    #binary{length = opts_get_value(length, Opts)};
type(Type, TypeOpts) ->
    {Type, TypeOpts}.

field(Name, Opts) ->
    ?O2R(#field{name = Name}, field, Opts).

object(Name, Opts) ->
    G = ?O2R(#object{name = Name}, struct, Opts),
    {Fields, NOpts} = elements(G#object.opts),
    G#object{fields = Fields, opts = NOpts}.

rpc(Name0, Opts) ->
    Name = binary:list_to_bin([get(prefix), $., Name0]),
    R = ?O2R(#rpc{name = Name}, rpc, Opts),
    {Fields, NOpts} = elements(R#rpc.opts),
    R#rpc{fields = Fields, opts = NOpts}.

opt_return({type,_,Type,Opts}) ->
    type(Type,Opts);
opt_return({input,_,_,Opts}) ->
    object(input, Opts);
opt_return({output,_,_,Opts}) ->
    object(output, Opts);
opt_return({'when',_,Value,_}) ->
    try
	{ok, Expr} = yang_xpath:parse(Value),
	Expr
    catch
	_:_ -> Value
    end;
opt_return({_,_,Value,_}) ->
    Value.

opts_keytake(Key, Opts, Default) ->
    case lists:keytake(Key, 1, Opts) of
	false ->
	    {Default, Opts};
	{value, Opt, NewOpts} ->
	    {opt_return(Opt), NewOpts}
    end.

%% this could be unroled through a parse_transform
opts_to_record(_, [], InOpts, Rec) ->
    N = tuple_size(Rec),
    erlang:setelement(N, Rec, InOpts);
opts_to_record(N, [Key|RecInfo], InOpts, Rec) ->
    {Value, NInOpts} = opts_keytake(Key, InOpts, erlang:element(N, Rec)),
    NRec = erlang:setelement(N, Rec, Value),
    opts_to_record(N + 1, RecInfo, NInOpts, NRec).
