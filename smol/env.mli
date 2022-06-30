exception Unbound_variable of { var : Name.t }

type env
type t = env

val empty : env
val enter : Var.t -> Type.t -> env -> env
val lookup : Name.t -> env -> Var.t * Type.t
