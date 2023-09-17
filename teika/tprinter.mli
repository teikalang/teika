open Ttree
open Terror

val pp_term : Format.formatter -> term -> unit
val pp_term_hole : Format.formatter -> term hole -> unit
val pp_subst : Format.formatter -> subst -> unit
val pp_error : Format.formatter -> error -> unit