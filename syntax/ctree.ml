type term =
  (* TODO: printer location *)
  | CT_loc of { term : term; loc : Location.t [@opaque] }
  | CT_var of { var : Name.t }
  | CT_extension of { extension : Name.t }
  | CT_forall of { param : term; return : term }
  | CT_lambda of { param : term; return : term }
  | CT_apply of { lambda : term; arg : term }
  | CT_self of { self : term; body : term }
  | CT_fix of { self : term; body : term }
  | CT_unroll of { term : term }
  | CT_pair of { left : term; right : term }
  | CT_both of { left : term; right : term }
  | CT_bind of { bound : term; value : term }
  | CT_semi of { left : term; right : term }
  | CT_annot of { value : term; annot : term }
  | CT_string of { literal : string }
  | CT_parens of { content : term }
  | CT_braces of { content : term }
[@@deriving show { with_path = false }]

let ct_loc loc term = CT_loc { loc; term }
let ct_var loc ~var = ct_loc loc (CT_var { var })
let ct_extension loc ~extension = ct_loc loc (CT_extension { extension })
let ct_forall loc ~param ~return = ct_loc loc (CT_forall { param; return })
let ct_lambda loc ~param ~return = ct_loc loc (CT_lambda { param; return })
let ct_apply loc ~lambda ~arg = ct_loc loc (CT_apply { lambda; arg })
let ct_self loc ~self ~body = ct_loc loc (CT_self { self; body })
let ct_fix loc ~self ~body = ct_loc loc (CT_fix { self; body })
let ct_unroll loc ~term = ct_loc loc (CT_unroll { term })
let ct_pair loc ~left ~right = ct_loc loc (CT_pair { left; right })
let ct_both loc ~left ~right = ct_loc loc (CT_both { left; right })
let ct_bind loc ~bound ~value = ct_loc loc (CT_bind { bound; value })
let ct_semi loc ~left ~right = ct_loc loc (CT_semi { left; right })
let ct_annot loc ~value ~annot = ct_loc loc (CT_annot { value; annot })
let ct_string loc ~literal = ct_loc loc (CT_string { literal })
let ct_parens loc ~content = ct_loc loc (CT_parens { content })
let ct_braces loc ~content = ct_loc loc (CT_braces { content })
