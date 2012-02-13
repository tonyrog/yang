%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    YANG token scanner
%%% @end
%%% Created :  4 Jan 2012 by Tony Rogvall <tony@rogvall.se>

-module(yang_scan_nif).

-on_load(init/0).
-export([open/1, open/2, string/1, next/1, push_back/2, close/1]).
-export([all/1, all/2]).
-export([file/1, file/2]).
%% nif exports
-export([new/0, next_token/1, next_token/2]).

-import(lists, [reverse/1]).

-type line() :: pos_integer().

-type token() :: {string,line(),string()} |
		 {word,line(),string()} |
		 {'{',line()} |
		 {'}',line()} |
		 {';',line()}.


-record(yang_scan,
	{
	  tokens = [],     %% token buffer
	  scanner,         %% nif scanner state
	  stream           %% stream data
	}).

init() ->
    Nif = filename:join(code:priv_dir(yang),"yang_drv"),
    io:format("Loading: ~s\n", [Nif]),
    erlang:load_nif(Nif, 0).

new() ->
    erlang:error(nif_not_loaded).

next_token(_Scanner) ->    
    erlang:error(nif_not_loaded).

next_token(_Scanner,_Binary) ->
    erlang:error(nif_not_loaded).


file(File) ->
    file(File, []).
file(File, Opts) ->
    case open(File, Opts) of
	{ok,Scan} ->
	    Res = all(Scan),
	    close(Scan),
	    Res;
	Error ->
	    Error
    end.
	    
open(File) ->
    open(File, []).

open(File,Opts) ->
    ChunkSize = proplists:get_value(chunk_size, Opts, 1024),
    case file:open(File, [read,raw,binary]) of
	{ok,Fd} ->
	    Scanner = new(),
	    {ok, #yang_scan { scanner = Scanner, stream={Fd,ChunkSize}}};
	Error ->
	    Error
    end.

string(Binary) when is_binary(Binary) ->
    Scanner = new(),
    Token = next_token(Scanner, Binary),
    %% FIXME: check for more!
    {ok, #yang_scan { scanner=Scanner, tokens=[Token] }};
string(List) when is_list(List) ->
    Scanner = new(),
    Token = next_token(Scanner, list_to_binary(List)),
    %% FIXME: check for more!
    {ok, #yang_scan { scanner=Scanner, tokens=[Token] }}.

close(#yang_scan { stream=undefined }) ->
    ok;
close(#yang_scan { stream={Fd,_} }) ->
    %% cleanup scanner ? this is now delayed until garbage collection
    file:close(Fd).

all(Scan) ->
    all(Scan, []).

all(Scan, Acc) ->
    case next(Scan) of
	Error = {error,_} ->
	    Error;
	eof ->
	    {ok,reverse(Acc)};
	{Token,Scan1} ->
	    all(Scan1,[Token|Acc])
    end.

%% @doc
%%    Unread a token from the token strem.
%% @end

-spec push_back(Token::token(), Scan::#yang_scan{}) ->
		       #yang_scan{}.

push_back(Token, Scan = #yang_scan { tokens=Ts}) ->
    Scan#yang_scan { tokens=[Token|Ts] }.

%% @doc
%%    Read next token, while handle string + string cases
%% @end
-spec next(Scan::#yang_scan{}) ->
		  {token(), #yang_scan{}} |
		  eof |
		  {error, term()}.
		  
next(Scan) ->
    case load_token(Scan) of
	{Token={string,_,_},Scan1} ->
	    next_(Scan1, Token);
	Other ->
	    Other
    end.

next_(Scan, StringToken) ->
    case load_token(Scan) of
	{PlusToken={word,_,<<"+">>},Scan1} ->
	    next_(Scan1, StringToken, PlusToken);
	Error = {error,_} ->
	    Error;
	{Token,Scan1} ->
	    {StringToken,Scan1#yang_scan { tokens=[Token] }};
	eof ->
	    {StringToken,Scan#yang_scan { tokens=[eof] }}
    end.

next_(Scan, StringToken={string,Ln,Str1}, PlusToken) ->
    case load_token(Scan) of
	{{string,_,Str2},Scan1} ->
	    next_(Scan1, {string,Ln,<<Str1/binary,Str2/binary>>});
	Error = {error,_} ->
	    Error;
	{Token,Scan1} ->
	    {StringToken,Scan1#yang_scan { tokens=[PlusToken,Token] }};
	eof ->
	    {StringToken,Scan#yang_scan { tokens=[PlusToken,eof] }}
    end.

load_token(Y=#yang_scan { tokens=[T|Ts]}) ->
    {T, Y#yang_scan { tokens=Ts }};
load_token(Y=#yang_scan { stream=S,tokens=[],scanner=Scanner}) ->
    case next_token(Scanner) of
	more ->
	    case load_more(Scanner, S) of
		eof -> eof;
		Error = {error,_} ->Error;
		Token -> {Token, Y}
	    end;
	Token ->
	    {Token,Y}
    end.

load_more(Scanner, S) ->
    case read(S) of
	{ok,Binary} ->
	    case next_token(Scanner, Binary) of
		more ->
		    load_more(Scanner, S);
		Token ->
		    Token
	    end;
	Reason ->
	    Reason
    end.
    

read(undefined) ->
    eof;
read({Fd,Size}) ->
    case file:read(Fd, Size) of
	{ok,Bin} -> {ok,Bin};
	Error -> Error
    end.

