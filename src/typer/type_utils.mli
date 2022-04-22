open Type

val in_type : var:type_ -> type_ -> bool
val forall_vars : forall:Forall_id.t -> type_ -> type_ list
val free_vars_in_env : Env.t -> type_ -> type_ list
