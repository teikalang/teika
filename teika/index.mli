type index
type t = index [@@deriving show, eq]

val zero : index
val one : index

(* repr *)
(* TODO: this API is non ideal *)
val of_int : int -> index
val repr : index -> int

(* operations *)
val ( + ) : index -> index -> index
val ( - ) : index -> index -> index
val ( < ) : index -> index -> bool
val ( > ) : index -> index -> bool
