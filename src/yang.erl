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
%%%     YANG scanner & parser
%%% @end
%%% Created :  2 Jan 2012 by Tony Rogvall <tony@rogvall.se>

-module(yang).

-export([scan_file/1,
	 parse_file/1,
	 deep_parse_file/1,
	 validate_file/1,
	 json_rpc/1,
	 check_type/2]).
-export([bin_hook/1]).
-import(lists, [reverse/1]).

-type  type() :: binary() | {type,integer(),binary(),list()}.


scan_file(File) ->
    yang_scan:file(File).

parse_file(File) ->
    yang_parser:parse(File).

deep_parse_file(File) ->
    yang_parser:deep_parse_file(File).

validate_file(File) ->
    yang_parser:validate(File).

json_rpc(YangFile) ->
    yang_json:json_rpc(YangFile).


bin_hook(L) when is_list(L) ->
    fun(F, Opts) ->
	    case lists:keyfind(filename:basename(F), 1, L) of
		{_, B} when is_binary(B) ->
		    bin_open_file(F, B, Opts);
		{_, Fn} when is_function(Fn, 0) ->
		    bin_open_file(F, Fn(), Opts);
		false ->
		    {error, enoent}
	    end
    end.

bin_open_file(F, Bin, _Opts) when is_binary(Bin) ->
    case file:open(F, [read, write, ram, binary]) of
	{ok, Fd} ->
	    ok = file:write(Fd, Bin),
	    {ok,0} = file:position(Fd,bof),
            {ok, Fd};
        {error, _} ->
            {error, enoent}
    end;
bin_open_file(_, _, _) ->
    {error, einval}.


-spec check_type(any(), type()) -> {true, any()} | false.
%% @spec check_type(Value, Type) -> {true, ConvertedValue} | false
%% Check Value against a YANG type definition.
%%
%% The "types" object, array and untyped are not actually YANG types, but used in the
%% YANG_JSON spec. They are only included here initially; it should be an error to call
%% this function for such objects (except perhaps for "untyped", which indicates that no
%% YANG type could be found).
%% @end
check_type(X, <<"object">> ) -> {true, X};
check_type(X, <<"array">>  ) -> {true, X};
check_type(X, undefined) -> {true, X};
check_type(X, anyxml) when is_list(X); is_binary(X) -> {true, X};
check_type(X0, {type,_,<<"enumeration">>,En}) ->
    try
	X = if is_integer(X0) -> list_to_binary(integer_to_list(X0));
	       is_binary(X0) -> X0;
	       is_list(X0) -> list_to_binary(X0)
	    end,
	case find_enum(En, X) of
	    [] -> false;
	    [{Key, _}] ->
		{true, Key};
	    [{K1, V1}, {K2, V2}] ->
		%% Either V1 or V2 must be equal to X, or it's an error
		if V1 == X -> {true, K1};
		   V2 == X -> {true, K2}
		end
	end
    catch error:_ -> false
    end;
check_type(X, {type,_,<<"binary">>,_}) when is_binary(X) -> {true, X};
check_type(X, {type,_,<<"bits">>,_}) when is_bitstring(X) -> {true, X};
check_type(X, {type,_,<<"string">>,_}) ->
    try {true, iolist_to_binary(X)}
    catch
	error:_ ->
	    false
    end;
check_type(<<"1">>, {type,_,<<"boolean">>,_}) -> {true, true};
check_type(<<"0">>, {type,_,<<"boolean">>,_}) -> {true, false};
check_type(X, {type,_,<<"empty">>,_} ) ->
    case is_empty(X) of
	true -> {true, []};
	false -> false
    end;
check_type(X, {type,_,<<"decimal64">>,_}) ->
    case re:split(X, "\\.", [{return,list}]) of
	[_Ib, _Fb]  ->
	    case io_lib:fread("~f", binary_to_list(X)) of
		{ok, [F], []} ->
		    {true, F};
		_ ->
		    false
	    end;
	_ ->
	    false
    end;
check_type(X, {type,_,<<"int", _/binary>> = T,_}) when is_integer(X) ->
    check_int_type(X, T);
check_type(X, {type,_,<<"int", _/binary>> = T,_}) ->
    try check_int_type(list_to_integer(to_list(X)), T)
    catch
	error:_ ->
	    false
    end;
check_type(X, {type,_,<<"uint", _/binary>> = T,_}) when is_integer(X) ->
    check_uint_type(X, T);
check_type(X, {type,_,<<"uint", _/binary>> = T,_}) ->
    try check_uint_type(list_to_integer(to_list(X)), T)
    catch
	error:_ ->
	    false
    end;
check_type(X, _) ->
    %% TODO: cover all types
    {true, X}.

check_int_type(N, T) when is_integer(N) ->
    if 	T == <<"uint8">>, 08 =< N, N =< 255 -> {true, N};
	T == <<"uint16">>, 0 =< N, N =< 65535 -> {true, N};
	T == <<"uint32">>, 0 =< N, N =< 4294967295 -> {true, N};
	T == <<"uint64">>, 0 =< N, N =< 18446744073709551615 -> {true, N};
	true -> false
    end.

check_uint_type(N, T) when is_integer(N) ->
    if T == <<"uint8">>, 08 =< N, N =< 255 -> {true, N};
       T == <<"uint16">>, 0 =< N, N =< 65535 -> {true, N};
       T == <<"uint32">>, 0 =< N, N =< 4294967295 -> {true, N};
       T == <<"uint64">>, 0 =< N, N =< 18446744073709551615 -> {true, N};
       true -> false
    end.

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


to_list(B) when is_binary(B) ->
    binary_to_list(B);
to_list(L) when is_list(L) ->
    L.

get_en_value(I) ->
    {value, _, V, _} = lists:keyfind(value, 1, I),
    V.

is_empty(<<>>) -> true;
is_empty([])   -> true;
is_empty(_)    -> false.

