open Type

val in_type : var:type_ -> type_ -> bool
val forall_vars : forall:Forall_id.t -> type_ -> type_ list
val weak_vars : type_ -> type_ list
