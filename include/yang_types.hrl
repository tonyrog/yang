%%%---- BEGIN COPYRIGHT -------------------------------------------------------
%%%
%%% Copyright (C) 2012 Feuerlabs, Inc. All rights reserved.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at http://mozilla.org/MPL/2.0/.
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
