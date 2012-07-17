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
%%
%% Common YANG type specs
%%
-ifndef(__YANG_TYPES_HRL_).
-define(__YANG_TYPES_HRL_, true).

-type yang_stmt_name() :: atom() | binary().
-type yang_stmt_arg()  :: atom() | string() | binary().
-type yang_statement() :: {yang_stmt_name(),integer(),
			   yang_stmt_arg(),[yang_statement()]}.

-endif.
