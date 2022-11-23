open Ttree
open Context

val normalize_term : term -> term Normalize_context(Subst).t
val normalize_type : type_ -> type_ Normalize_context(Subst).t
