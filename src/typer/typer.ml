let () =
  (* recursive modules *)
  Transl_type.type_pat_ref := Type_pat.type_pat

module Env = Env
module Unify = Unify
module Tree = Tree

let transl_type = Transl_type.transl_type
let type_pat = Type_pat.type_pat
let type_expr = Type_expr.type_expr
