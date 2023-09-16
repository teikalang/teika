open Ttree
open Context

val tt_expand_subst : subst:subst -> term -> term
val tt_expand_head : term -> term
val tt_escape_check : term -> unit Var_context.t
val tt_unfold_fix : term -> term
