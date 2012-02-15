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
