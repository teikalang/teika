type t [@@deriving show]

val generic : t
(** [generic] is a rank below initial, it's used to represent a bound but
    untagged forall *)

val initial : t
val next : t -> t
val ( > ) : t -> t -> bool
val ( >= ) : t -> t -> bool
