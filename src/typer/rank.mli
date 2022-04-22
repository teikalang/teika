type t [@@deriving show]

val initial : t
val next : t -> t
val ( > ) : t -> t -> bool
val ( >= ) : t -> t -> bool
