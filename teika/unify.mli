open Ttree

val unify_term : expected:term -> received:term -> unit Context.t
val unify_type : expected:type_ -> received:type_ -> unit Context.t
