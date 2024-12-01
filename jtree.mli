open Utils

module Var : sig
  type var = private Global of Name.t | Funct of Level.t | Local of Level.t
  type t = var

  val main : var
  val type_stub : var
  val funct : Level.t -> var
  val local : Level.t -> var
end

type program = statement list
and statement = JS_const of { name : Var.t; init : expression }
and block = JBlock of { statements : statement list; return : expression }

and expression =
  | JE_loc of { expression : expression; loc : Location.t }
  | JE_var of { var : Var.t }
  | JE_arrow of { params : Var.t list; body : expression }
  | JE_generator of { params : Var.t list; body : block }
  (* TODO: not really a lambda and arg *)
  | JE_new of { constructor : expression }
  | JE_call of { lambda : expression; args : expression list }
  | JE_yield of { expression : expression }
  | JE_string of { literal : string }
