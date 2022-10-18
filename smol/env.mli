exception Unbound_variable of { var : Name.t }

type env
type t = env

val initial : env
val enter : Var.t -> Term.t -> env -> env
val lookup : Name.t -> env -> Term.t
