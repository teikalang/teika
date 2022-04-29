open Type

val pp_type : Format.formatter -> type_ -> unit
val pp_type_debug : Format.formatter -> type_ -> unit

val with_pp_type :
  ?debug:bool -> ((Format.formatter -> type_ -> unit) -> unit) -> unit
