open Ttree

val tt_apply_subst : term -> subst -> term
val tt_open : term -> to_:term -> term
val tt_close : term -> from:Level.t -> term
val ts_inverse : subst -> subst
