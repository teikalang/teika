open Ttree
open Context

val unify_term :
  expected:term -> received:term -> unit Unify_context(Instance)(Normalize).t

val unify_type :
  expected:type_ -> received:type_ -> unit Unify_context(Instance)(Normalize).t
