open Syntax

type context

(* TODO: couple all the initial contexts *)
val initial : context
val solve_term : context -> Ctree.term -> (Ttree.term, exn) result
