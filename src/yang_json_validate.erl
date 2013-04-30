-module(yang_json_validate).

-export([validate/3, validate/4, validate_type/3, string_to_datetime/1]).

-include("typespec.hrl").

validate(ModuleSpec, Type, Doc) ->
    validate(ModuleSpec, Type, -1, Doc).

validate(ModuleSpec, Type, Depth, Doc) ->
    validate_(yang_typespec:get_type(ModuleSpec, Type), Depth, Doc).

validate_type(Type, Depth, Doc) ->
    validate_(Type, Depth, Doc).
    
%%------------------------------
%% internal functions
%%------------------------------

validate_(false, _, _) ->
    throw({error, unknown_type});
validate_(#object{fields = Fields}, Depth, {Doc}) when is_list(Doc) ->
    validate_fields(Fields, Depth, Doc);
validate_(#object{fields = Fields}, Depth, Doc) when is_list(Doc) ->
    validate_fields(Fields, Depth, Doc);
validate_(_Type, _, _Doc) ->
    throw({error, not_impleted_yet}).

%% stop here
validate_fields(_, 0, Doc) -> Doc;
validate_fields(Fields, Depth, Doc) ->
    validate_fields(Fields, Depth, Doc, []).

validate_fields([], _, [], NDoc) ->
    lists:reverse(NDoc);
validate_fields([], _, Doc, NDoc) ->
    error_logger:error_report([{function, validate_fields},
			       {remaining, Doc}, {done, NDoc}]),
    throw({error, too_much_fields, Doc});
validate_fields([Type|Tail], Depth, Doc, NDoc) ->
    N = element(2, Type),
    {Field, Doc1} = case lists:keytake(N, 1, Doc) of
        {value, F, D} ->
            {F, D};
        _ ->
            {false, Doc}
    end,
    case validate_item(N, Field, Depth, Type) of
        undefined ->
            validate_fields(Tail, Depth, Doc1, NDoc);
        NewField  ->
            validate_fields(Tail, Depth, Doc1, [NewField|NDoc])
    end.

%% validate_fields(Fields, Depth, Doc) ->
%%     lists:map(validate_field(_, Depth, Doc), Fields).

%% validate_field(Type, Depth, Doc) ->
%%     N = element(2, Type),
%%     validate_item(N, lists:keyfind(N, 1, Doc), Depth, Type).

%% stop here
validate_item(_, V, 0, _) -> V;

%%------------------------------
%% mandatory handling
%%------------------------------

validate_item(_, false, _, #field{mandatory = false, default = undefined}) ->
    undefined;
validate_item(_, false, _, #field{mandatory = false, name = N, default = Default}) ->
    {N, Default};
validate_item(_, false, _, #array{mandatory = false}) ->
    undefined;
validate_item(N, false, _, Type) when element(1, Type) =/= <<"boolean">> ->
    throw({error, missing_field, N});

%%------------------------------
%% complex types
%%------------------------------

validate_item(_, {N, V}, Depth, #field{type = Type}) ->
    {N, validate_item(N, V, Depth, Type)};
validate_item(_, {N, V}, Depth, #array{type = Type})
  when is_list(V) ->
    Fun = fun(Item) ->
            validate_item(N, Item, Depth - 1, Type)
    end,
    {N, lists:map(Fun, V)};
validate_item(_, {N, {V}}, Depth, #object{fields = Fields})
  when is_list(V) ->
    {N, {validate_fields(Fields, Depth - 1, V)}};
validate_item(_, {V}, Depth, #struct{fields = Fields})
  when is_list(V) ->
    {validate_fields(Fields, Depth, V)};

validate_item(N, V, _, Type = #enumeration{enum = Enum}) ->
    case lists:member(V, Enum) of
	true -> V;
	_    -> invalid_item(N, V, Type)
    end;

%%------------------------------
%% simple types
%%------------------------------

validate_item(_, V, _, #string{})
  when is_binary(V) -> V;

validate_item(_, V, _, {<<"int8">>, _})
  when is_integer(V), V >= -128, V =< 127 -> V;
validate_item(_, V, _, {<<"int16">>, _})
  when is_integer(V), V >= -32768, V =< 32767 -> V;
validate_item(_, V, _, {<<"int32">>, _})
  when is_integer(V), V >= -2147483648, V =< 2147483647 -> V;
validate_item(_, V, _, {<<"int64">>, _})
  when is_integer(V), V >= -9223372036854775808, V =< 9223372036854775807 -> V;

validate_item(_, V, _, {<<"uint8">>, _})
  when is_integer(V), V >= 0, V =< 255 -> V;
validate_item(_, V, _, {<<"uint16">>, _})
  when is_integer(V), V >= 0, V =< 65535 -> V;
validate_item(_, V, _, {<<"uint32">>, _})
  when is_integer(V), V >= 0, V =< 4294967295 -> V;
validate_item(_, V, _, {<<"uint64">>, _})
  when is_integer(V), V >= 0, V =< 18446744073709551615 -> V;

validate_item(_, V, _, {<<"boolean">>, _})
  when is_boolean(V) -> V;

%%------------------------------
%% non standard simple types
%%------------------------------
validate_item(_, V, _, {<<"number">>, _})
  when is_number(V) -> V;
validate_item(_, V, _, {<<"integer">>, _})
  when is_integer(V) -> V;

%%------------------------------
%% non standard complex types
%%------------------------------
validate_item(_, {V}, _, {<<"object">>, _})
  when is_list(V) ->
    {V};
validate_item(N, V, _, Type = {<<"timestamp">>, _})
  when is_list(V); is_binary(V) ->
    case string_to_datetime(V) of
        {ok, _} ->
            V;
        error ->
            invalid_item(N, V, Type)
    end;

validate_item(N, V, _, Type) ->
    invalid_item(N, V, Type).

invalid_item(N, V, Type) ->
    error_logger:error_report([{function, validate_item},
			      {field, N}, {value, V}, {type, Type}]),
    throw({error, invalid_type, {{N, V}, Type}}).

%%------------------------------

-spec string_to_datetime(string()) -> calendar:datetime().
string_to_datetime(Date) ->
    case re:run(Date, "^([0-9]{4})(-?)([0-9]{2})(-?)([0-9]{2})([tT])(.*)$", [{capture, all_but_first, list}]) of
        {match, [Year, Cut1, Month, Cut2, Day, T, TimeString]} when (((T == "t") or (T == "T")) and (Cut1 == Cut2)) ->
            case re:run(TimeString, "^([0-9]{2})(:?)([0-9]{2})(:?)([0-9]{2})(.*)$", [{capture, all_but_first, list}]) of
                {match, [Hour, Cut3, Minute, Cut4, Second, TimeZoneString]} when (Cut3 == Cut4) ->
                    validate_datetime({s2i(Year), s2i(Month), s2i(Day)}, {s2i(Hour), s2i(Minute), s2i(Second)}, TimeZoneString);
                _ ->
                    error
            end;
        _ -> error
    end.

validate_datetime({Year, Month, Day} = DateTuple, {Hour, Minute, Second} = TimeTuple, TimeZoneString) ->
    case {calendar:valid_date(Year, Month, Day), valid_time(Hour, Minute, Second)} of
        {true, true} ->
            check_time_zone(DateTuple, TimeTuple, TimeZoneString);
        _ ->
            error
    end.

check_time_zone(DateTuple, TimeTuple, TimeZoneString) ->
    case re:run(TimeZoneString, "^([Z+-])([0-9]{2})?(:?)([0-9]{2})?$", [{capture, all_but_first, list}]) of
        {match, ["Z", "", ""]} ->
            {ok, {DateTuple, TimeTuple}};
        {match, [T, Time | Other]} when ((T == "+") or (T == "-")) ->
            timezone_offset({DateTuple, TimeTuple}, T, s2i(Time), Other);
        _ ->
            error
    end.

valid_time(H, M, S) when (H >= 0) and (H =< 23) and (M >= 0) and (M =< 59) and (S >= 0) and (S =< 59) ->
    true;
valid_time(_, _, _) ->
    false.

s2i("") -> 0;
s2i(Str) -> list_to_integer(Str).

timezone_offset(DateTime, T, Hours, PossiblyMinutes) when (Hours >= 0) and (Hours =< 23)  ->
    Seconds1 = calendar:datetime_to_gregorian_seconds(DateTime),
    Seconds2 = tz_fix(T, Seconds1, Hours * 3600),
    case PossiblyMinutes of
        [""] ->
            {ok, calendar:gregorian_seconds_to_datetime(Seconds2)};
        [_Cut, MinutesString] ->
            Minutes = s2i(MinutesString),
            case (Minutes >= 0) and (Minutes =< 59) of
                true ->
                    Seconds3 = tz_fix(T, Seconds2, Minutes * 60),
                    {ok, calendar:gregorian_seconds_to_datetime(Seconds3)};
                false ->
                    error
            end
    end;
timezone_offset(_DateTime, _, _, _) -> error.

tz_fix("+", A, B) -> A - B;
tz_fix("-", A, B) -> A + B.
