-module(rebar_yang_compiler).
-export([pre_compile/2, clean/2]).

-define(YANGDIR, filename:join(["src", "yang"])).
-define(YANGBUILDDIR, filename:join(["src", "yang", "hrl"])).


%% ===================================================================
%% Public API
%% ===================================================================

-spec pre_compile(rebar_config:config(), file:filename()) -> ok.
pre_compile(Config, _Appfile) ->
    Opts = rebar_config:get(Config, yang_opts, []),
    YangDir = proplists:get_value(yang_dir, Opts, ?YANGDIR),
    YangBuildDir = proplists:get_value(yang_build_dir, Opts, ?YANGBUILDDIR),
    rebar_base_compiler:run(Config, filelib:wildcard(filename:join([YangDir, "*.yang"])),
                            YangDir, ".yang", YangBuildDir, ".hrl", fun compile_yang/3).

-spec clean(rebar_config:config(), file:filename()) -> ok.
clean(Config, _AppFile) ->
    Opts = rebar_config:get(Config, yang_opts, []),
    YangBuildDir = proplists:get_value(yang_build_dir, Opts, ?YANGBUILDDIR),
    rebar_file_utils:delete_each(filelib:wildcard(filename:join([YangBuildDir, "*.hrl"]))).

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec compile_yang(In :: file:filename(), Out :: file:filename(), Config :: rebar_config:config()) ->
    ok | {error, Error :: term()}.
compile_yang(In, Out, Config) ->
    case yang:deep_parse_file(In) of
        {ok, Yang} ->
            Ts = yang:typespec(Yang),
            ok = filelib:ensure_dir(Out), 
            file:write_file(Out, yang_typespec:hrl(Ts));
        {error, Error} = E->
            rebar_log:log(error, "Error: ~p~n", [Error]), E
    end.
