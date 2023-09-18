open Ttree
open Context

val tt_repr : term -> term
val tt_match : term -> term
val tt_apply_subst : term -> subst -> term
val tt_open : term -> to_:term -> term
val tt_close : term -> from:Level.t -> term
val tt_expand_head : aliases:term Level.Map.t -> term -> term
val tt_escape_check : aliases:term Level.Map.t -> term -> unit Var_context.t
val tt_unfold_fix : aliases:term Level.Map.t -> term -> term
val ts_inverse : subst -> subst