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
%%%    YANG token scanner, reference implementation
%%% @end
%%% Created :  4 Jan 2012 by Tony Rogvall <tony@rogvall.se>

-module(yang_scan_erl).

-export([open/1, open/2, string/1, next/1, push_back/2, close/1]).
-export([all/1, all/2]).
-export([file/1, file/2]).
-export([open_file/2, open_options/0]).    % also used by yang_scan_nif.erl
-import(lists, [reverse/1]).

-type line() :: pos_integer().

-type token() :: {string,line(),binary()} |
		 {word,line(),binary()} |
		 {'{',line()} |
		 {'}',line()} |
		 {';',line()}.


-record(yang_scan,
	{
	  buffer = [],     %% list chunk
	  tokens = [],     %% token buffer
	  line = 1,        %% current line number
	  column = 1,      %% current column
	  stream           %% stream data
	}).

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
    case open_file(File, Opts) of
	{ok,Fd} ->
	    {ok, #yang_scan { stream={Fd,ChunkSize}}};
	Error ->
	    Error
    end.

open_file(File, Opts) ->
    case lists:keyfind(open_hook, 1, Opts) of
	false ->
	    file:open(File, open_options());
	{_, F} when is_function(F, 2) ->
	    F(File, Opts)
    end.

open_options() ->
    [read, raw, binary].


string(Binary) when is_binary(Binary) ->
    {ok, #yang_scan { buffer=Binary }};
string(List) when is_list(List) ->
    {ok, #yang_scan { buffer=list_to_binary(List) }}.

close(#yang_scan { stream=undefined }) ->
    ok;
close(#yang_scan { stream={Fd,_} }) ->
    file:close(Fd).

all(Cont) ->
    all(Cont, []).

all(Cont, Acc) ->
    case next(Cont) of
	Error = {error,_} ->
	    Error;
	eof ->
	    {ok,reverse(Acc)};
	{Token,Cont1} ->
	    all(Cont1,[Token|Acc])
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

next(#yang_scan { buffer=B,line=L,column=C,stream=S,tokens=[] }) ->
    case wsp(B,L,C,S) of
	{Token={string,_,_},Scan1} ->
	    next_(Scan1, Token);
	Other ->
	    Other
    end;
next(#yang_scan { tokens=[eof] }) ->
    eof;
next(Scan = #yang_scan { tokens=[T|Ts] }) ->
    {T,Scan#yang_scan { tokens=Ts }}.

next_(Scan, StringToken) ->
    case wsp(Scan) of
	{PlusToken={word,_,"+"},Scan1} ->
	    next_(Scan1, StringToken, PlusToken);
	Error = {error,_} ->
	    Error;
	{Token,Scan1} ->
	    {StringToken,Scan1#yang_scan { tokens=[Token] }};
	eof ->
	    {StringToken,Scan#yang_scan { tokens=[eof] }}
    end.

next_(Scan, StringToken={string,Ln,Str1}, PlusToken) ->
    case wsp(Scan) of
	{{string,_,Str2},Scan1} ->
	    next_(Scan1, {string,Ln,Str1++Str2});
	Error = {error,_} ->
	    Error;
	{Token,Scan1} ->
	    {StringToken,Scan1#yang_scan { tokens=[PlusToken,Token] }};
	eof ->
	    {StringToken,Scan#yang_scan { tokens=[PlusToken,eof] }}
    end.


wsp(#yang_scan { buffer=B,line=L,column=C,stream=S}) ->
    wsp(B,L,C,S).

wsp([$/,$/|B],L,C,S) ->
    line_comment(B,L,C,S);
wsp([$/,$*|B],L,C,S) ->
    block_comment(B,L,C,S);
wsp([$/],L,C,S) ->
    case read(S) of
	{ok,B} -> wsp([$/|B],L,C,S);
	eof -> eof;
	Error -> Error
    end;
wsp([A|B],L,C,S) ->
    case A of
	$\s -> wsp(B,L,C+1,S);
	$\t -> wsp(B,L,C+8,S);
	$\r -> wsp(B,L,C,S);
	$\n -> wsp(B,L+1,1,S);
	$"  -> dquote_string(B,[],L,C,L,C+1,false,S);
	$'  -> squote_string(B,[],L,L,C+1,S);
	${  -> token_(B,'{',L,C+1,S);
	$}  -> token_(B, '}',L,C+1,S);
	$;  -> token_(B, ';',L,C+1,S);
	_   -> word(B,[A],L,C+1,S)
    end;
wsp([],L,C,S) ->
    case read(S) of
	{ok,B0} -> wsp(B0,L,C,S);
	eof -> eof;
	Error -> Error
    end.

word(B0 = [$/,$/|_B],Acc,L,C,S) ->
    word_(B0,Acc,L,C,S);
word(B0 = [$/,$*|_B],Acc,L,C,S) ->
    word_(B0,Acc,L,C,S);
word([$/],Acc,L,C,S) ->
    case read(S) of
	{ok,B} -> word([$/|B],Acc,L,C,S);
	eof -> eof;
	Error -> Error
    end;
word(B0 = [A|B],Acc,L,C,S) ->
    case A of
	$\s -> word_(B,Acc,L,C+1,S);
	$\t -> word_(B,Acc,L,C+8,S);
	$\r -> word_(B,Acc,L,C,S);
	$\n -> word_(B0,Acc,L,C,S);
	$;  -> word_(B0,Acc,L,C,S);
	${  -> word_(B0,Acc,L,C,S);
	$}  -> word_(B0,Acc,L,C,S);
	_ -> word(B, [A|Acc],L,C+1,S)
    end;
word([],Acc,L,C,S) ->
    case read(S) of
	{ok,B1} -> word(B1,Acc,L,C,S);
	eof -> word_([],Acc,L,C,S);
	Error -> Error
    end.

%% @doc
%%    scan double quoted string. "this is such a string"
%%    double quoted strings also handle \n,\t,\" and \\
%%    after finding a \n the code backtrack and remove
%%    all white space (\s\t) processeding the \n character,
%%    after that the scanner starts skip white space until
%%    C0 = Column of " is reached or non white space char are found
%% @end

%% dquote_string(Binary,Acc,StartLine,StartColumn,
%%               CurrentLine,CurrentColumn,WhiteSkipFlag,Stream)
%%

dquote_string([$\\,A|B],Acc,L0,C0,L,C,_W,S) ->
    case A of
	$n  -> dquote_string(B,[$\n|Acc],L0,C0,L,C,false,S);
	$t  -> dquote_string(B,[$\t|Acc],L0,C0,L,C,false,S);
	$"  -> dquote_string(B,[A|Acc],L0,L,C0,C,false,S);
	$\\ -> dquote_string(B,[A|Acc],L0,L,C0,C,false,S);
	_   -> dquote_string(B,[A,$\\|Acc],L0,C0,L,C,false,S)
    end;
dquote_string([$\\],Acc,L0,C0,L,C,W,S) ->
    case read(S) of
	{ok,B1} -> dquote_string([$\\|B1],Acc,L0,C0,L,C,W,S);
	eof -> {error,unterminated_string};
	Error -> Error
    end;
dquote_string([A|B],Acc,L0,C0,L,C,W,S) ->
    case A of
	$"  ->
	    string_(B,reverse(Acc),L0,L,C+1,S);
	$\n ->
	    Acc1 = trim_wsp(Acc),
	    dquote_string(B,[A|Acc1],L0,C0,L+1,1,true,S);
	$\s ->
	    if W, C =< C0 ->
		    dquote_string(B,Acc,L0,C0,L,C+1,W,S);
	       true ->
		    dquote_string(B,[A|Acc],L0,C0,L,C+1,false,S)
	    end;
	$\t ->
	    if W, C =< C0 ->
		    dquote_string(B,Acc,L0,C0,L,C+8,W,S);
	       true ->
		    dquote_string(B,[A|Acc],L0,C0,L,C+8,false,S)
	    end;
	_   ->
	    dquote_string(B,[A|Acc],L0,C0,L,C+1,false,S)
    end;
dquote_string([],Acc,L0,C0,L,C,W,S) ->
    case read(S) of
	{ok,B1} -> dquote_string(B1,Acc,L0,C0,L,C,W,S);
	eof -> {error,unterminated_string};
	Error -> Error
    end.



%% @doc
%%    scan single quoted string. 'this is such a string'
%% @end

squote_string([],Acc,L0,L,C,S) ->
    case read(S) of
	{ok,B1} -> squote_string(B1,Acc,L0,L,C,S);
	eof -> {error,unterminated_string};
	Error -> Error
    end;
squote_string([A|B],Acc,L0,L,C,S) ->
    case A of
	$'  -> string_(B,reverse(Acc),L0,L,C+1,S);
	$\n -> squote_string(B,[A|Acc],L0,L+1,1,S);
	_   -> squote_string(B,[A|Acc],L0,L,C+1,S)
    end.

%% @doc
%%   Skip line comment from // until \n
%% @end

line_comment([$\n|B],L,_C,S) ->
    wsp(B,L+1,1,S);
line_comment([_|B],L,C,S) ->
    line_comment(B,L,C+1,S);
line_comment([],L,C,S) ->
    case read(S) of
	{ok,B1} ->
	    line_comment(B1,L,C,S);
	eof -> eof;
	Error -> Error
    end.

%% @doc
%%   Skip block comment from /* until */
%% @end

block_comment([$*,$/|B],L,C,S) ->
    wsp(B,L,C+2,S);
block_comment([$*],L,C,S) ->
    case read(S) of
	{ok,B1} ->
	    block_comment([$*|B1],L,C,S);
	eof -> eof;
	Error -> Error
    end;
block_comment([$\n|B],L,_C,S) ->
    block_comment(B,L+1,1,S);
block_comment([_|B],L,C,S) ->
    block_comment(B,L,C+1,S);
block_comment([],L,C,S) ->
    case read(S) of
	{ok,B1} ->
	    block_comment(B1,L,C,S);
	eof -> eof;
	Error -> Error
    end.

%%
%% remove white space
%%
trim_wsp([$\s|Cs]) -> trim_wsp(Cs);
trim_wsp([$\t|Cs]) -> trim_wsp(Cs);
trim_wsp(Cs) -> Cs.


string_(B,Str,L0,L,C,S) ->
    {{string,L0,list_to_binary(Str)},
     #yang_scan { buffer=B,line=L,column=C,stream=S}}.

word_(B,Acc,L,C,S) ->
    {{word,L,list_to_binary(reverse(Acc))},
     #yang_scan { buffer=B,line=L,column=C,stream=S}}.

token_(B,Tok,L,C,S) ->
    {{Tok,L},
     #yang_scan { buffer=B,line=L,column=C,stream=S}}.

read(undefined) ->
    eof;
read({Fd,Size}) ->
    case file:read(Fd, Size) of
	{ok,Bin} -> {ok,binary_to_list(Bin)};
	Error -> Error
    end.

