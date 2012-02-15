%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%     YANG scanner & parser
%%% @end
%%% Created :  2 Jan 2012 by Tony Rogvall <tony@rogvall.se>

-module(yang).

-export([scan_file/1, parse_file/1, validate_file/1, json_rpc/1]).
-import(lists, [reverse/1]).

scan_file(File) -> 
    yang_scan:file(File).

parse_file(File) ->
    yang_parser:parse(File).

validate_file(File) ->
    yang_parser:validate(File).

json_rpc(YangFile) ->
    yang_json:json_rpc(YangFile).
