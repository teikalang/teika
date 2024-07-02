open Syntax

module Infer : sig
  val infer_term : Ctree.term -> (Ttree.term, Terror.error) result
end
