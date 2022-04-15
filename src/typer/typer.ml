let () =
  (* recursive modules *)
  Transl_type.type_pat_ref := Type_pat.type_pat

module Type = Type
module Env = Env
module Unify = Unify
module Tree = Tree

let pp_type = Print_type.pp_type
let with_pp_type = Print_type.with_pp_type
let transl_type = Transl_type.transl_type
let type_pat = Type_pat.type_pat
let type_expr = Type_expr.type_expr
