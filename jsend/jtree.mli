type expression =
  | JE_loc of { expression : expression; loc : Location.t }
  | JE_var of { var : Var.t }
  | JE_generator of { param : Var.t; return : expression }
  (* TODO: not really a lambda and arg *)
  | JE_call of { lambda : expression; arg : expression }
  | JE_yield of { expression : expression }
  | JE_string of { literal : string }
