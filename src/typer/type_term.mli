open Utils
open Type
open Tree

type error = Invalid_number | Not_a_type | Unimplemented

exception Error of { loc : Location.t; error : error }

val type_expr : Env.t -> Language.expr -> type_ * term
val type_type : Env.t -> Language.expr -> type_ * term

val type_pat :
  Env.t ->
  Language.pat ->
  Type.type_ * Tree.term_pat * (Name.t * Type.type_) list * Env.t
