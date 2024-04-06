open Syntax

module Infer : sig
  val infer_term : Ltree.term -> (Ttree.term, Terror.error) result
end
