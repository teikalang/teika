type literal = private L_string of string
type t = literal [@@deriving show]

val equal : literal -> literal -> bool
val l_string : string -> literal
