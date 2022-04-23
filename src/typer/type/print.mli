open Repr

val pp_type : Format.formatter -> type_ -> unit
val with_pp_type : ((Format.formatter -> type_ -> unit) -> unit) -> unit
