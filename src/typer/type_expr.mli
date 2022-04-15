(* typing core language expressions *)
val type_expr : Env.t -> Syntax.term -> Type.t * Tree.expr
