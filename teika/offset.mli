type offset
type t = offset [@@deriving show, eq]

val zero : offset
val one : offset

(* repr *)
(* TODO: this API is non ideal *)
val of_int : int -> offset
val repr : offset -> int

(* predef *)
val initial : offset
val type_ : offset

(* operations *)
val ( + ) : offset -> offset -> offset
val ( - ) : offset -> offset -> offset
val ( < ) : offset -> offset -> bool