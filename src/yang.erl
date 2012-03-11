%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     YANG scanner & parser
%%% @end
%%% Created :  2 Jan 2012 by Tony Rogvall <tony@rogvall.se>

-module(yang).

-export([scan_file/1, parse_file/1, validate_file/1, json_rpc/1, check_type/2]).
-import(lists, [reverse/1]).

-type  type() :: binary() | {type,integer(),binary(),list()}.


scan_file(File) ->
    yang_scan:file(File).

parse_file(File) ->
    yang_parser:parse(File).

validate_file(File) ->
    yang_parser:validate(File).

json_rpc(YangFile) ->
    yang_json:json_rpc(YangFile).

-spec check_type(any(), type()) -> boolean().
%% @spec check_type(Value, Type) -> boolean()
%% Check Value against a YANG type definition.
%%
%% The "types" object, array and untyped are not actually YANG types, but used in the
%% YANG_JSON spec. They are only included here initially; it should be an error to call
%% this function for such objects (except perhaps for "untyped", which indicates that no
%% YANG type could be found).
%% @end
check_type(_X, <<"object">> ) -> true;
check_type(_X, <<"array">>  ) -> true;
check_type(_X, <<"untyped">>) -> true;
check_type(X, {type,_,<<"enumeration">>,En}) ->
    lists:any(fun({enum_,_,I}) ->
		      X == get_value(I)
	      end, En);
check_type(X, {type,_,<<"binary">>,_}) when is_binary(X) -> true;
check_type(X, {type,_,<<"bits">>,_}) when is_bitstring(X) -> true;
check_type(X, {type,_,<<"boolean">>,_}) -> X == <<"1">> orelse X == <<"0">>;
check_type(X, {type,_,<<"empty">>,_} ) -> is_empty(X);
check_type(X, {type,_,<<"decimal64">>,_}) ->
    case re:split(X, "\\.", [{return,list}]) of
	[Ib, Fb]  ->
	    %% For now, we ignore fraction-digits and range checking, but at least
	    %% verify that it's a decimal number
	    try   _ = list_to_integer(Ib),
		  _ = list_to_integer(Fb),
		     %% TODO: range checking
		 true
	    catch
		error:_ ->
		    false
	    end;
	_ ->
	    false
    end;
check_type(X, {type,_,<<"int", _/binary>> = T,_}) when is_integer(X) ->
    check_int_type(X, T);
check_type(X, {type,_,<<"int", _/binary>> = T,_}) ->
    try check_int_type(list_to_integer(binary_to_list(X)), T)
    catch
	error:_ ->
	    false
    end;
check_type(X, {type,_,<<"uint", _/binary>> = T,_}) when is_integer(X) ->
    check_uint_type(X, T);
check_type(X, {type,_,<<"uint", _/binary>> = T,_}) ->
    try check_uint_type(list_to_integer(binary_to_list(X)), T)
    catch
	error:_ ->
	    false
    end;
check_type(_, _) ->
    %% TODO: cover all types
    true.

check_int_type(N, T) when is_integer(N) ->
    if 	T == <<"uint8">>, 08 =< N, N =< 255 -> true;
	T == <<"uint16">>, 0 =< N, N =< 65535 -> true;
	T == <<"uint32">>, 0 =< N, N =< 4294967295 -> true;
	T == <<"uint64">>, 0 =< N, N =< 18446744073709551615 -> true;
	true -> false
    end.

check_uint_type(N, T) when is_integer(N) ->
    if T == <<"uint8">>, 08 =< N, N =< 255 -> true;
       T == <<"uint16">>, 0 =< N, N =< 65535 -> true;
       T == <<"uint32">>, 0 =< N, N =< 4294967295 -> true;
       T == <<"uint64">>, 0 =< N, N =< 18446744073709551615 -> true;
       true -> false
    end.


get_value(I) ->
    {value, V} = lists:keyfind(value, 1, I),
    V.

is_empty(<<>>) -> true;
is_empty([])   -> true;
is_empty(_)    -> false.
