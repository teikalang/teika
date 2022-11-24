open Ttree
open Context

val instance_term : term -> term Instance_context.t
val instance_type : type_ -> type_ Instance_context.t
val instance_desc : term_desc -> term_desc Instance_context.t
