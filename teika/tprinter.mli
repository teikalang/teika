open Format
open Ttree
open Terror

val pp_term : formatter -> term -> unit
val pp_pat : formatter -> pat -> unit
val pp_error : Format.formatter -> error -> unit
