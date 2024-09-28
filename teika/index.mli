type index = private int
type t = index [@@deriving show, eq]

val zero : index
val next : index -> index
val of_int : int -> index option
