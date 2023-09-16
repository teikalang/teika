open Ttree
open Context

val tt_repr : term -> term
val tt_match : term -> term
val tt_apply_subst : term -> subst -> term
val tt_open : term -> to_:term -> term
val tt_close : term -> from:Level.t -> term
val tt_expand_head : term -> term
val tt_escape_check : term -> unit Var_context.t
val tt_unfold_fix : term -> term
val ts_inverse : subst -> subst