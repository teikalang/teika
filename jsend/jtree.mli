type expression =
  | JE_loc of { expression : expression; loc : Location.t }
  | JE_var of { var : Var.t }
  | JE_generator of { params : Var.t list; block : block }
  (* TODO: not really a lambda and arg *)
  | JE_new of { constructor : expression }
  | JE_call of { lambda : expression; args : expression list }
  | JE_yield of { expression : expression }
  | JE_string of { literal : string }

and block =
  | JBlock of { consts : (Var.t * expression) list; return : expression }
