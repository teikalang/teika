type document
type t = document

val teika : version:int -> text:string -> document
val with_change : version:int -> text:string -> document -> document
