type document
type t = document
type diagnostic = { loc : Location.t; message : string }

val diagnostics : document -> diagnostic list
val teika : version:int -> text:string -> document
val with_change : version:int -> text:string -> document -> document
