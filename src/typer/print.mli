val pp_kind : Format.formatter -> Kind.t -> unit
val pp_type : Format.formatter -> Type.t -> unit
val pp_type_debug : Format.formatter -> Type.t -> unit

val with_pp_type :
  ?debug:bool -> ((Format.formatter -> Type.t -> unit) -> unit) -> unit
