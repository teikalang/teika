type term =
  (* TODO: printer location *)
  | ST_loc of { term : term; loc : Location.t [@opaque] }
  | ST_var of { var : Name.t }
  | ST_extension of { extension : Name.t }
  | ST_forall of { param : term; return : term }
  | ST_lambda of { param : term; return : term }
  | ST_apply of { lambda : term; arg : term }
  | ST_self of { self : term; body : term }
  | ST_fix of { self : term; body : term }
  | ST_unroll of { term : term }
  | ST_pair of { left : term; right : term }
  | ST_both of { left : term; right : term }
  | ST_bind of { bound : term; value : term }
  | ST_semi of { left : term; right : term }
  | ST_annot of { value : term; annot : term }
  | ST_string of { literal : string }
  | ST_parens of { content : term }
  | ST_braces of { content : term }
[@@deriving show { with_path = false }]

let st_loc loc term = ST_loc { loc; term }
let st_var loc ~var = st_loc loc (ST_var { var })
let st_extension loc ~extension = st_loc loc (ST_extension { extension })
let st_forall loc ~param ~return = st_loc loc (ST_forall { param; return })
let st_lambda loc ~param ~return = st_loc loc (ST_lambda { param; return })
let st_apply loc ~lambda ~arg = st_loc loc (ST_apply { lambda; arg })
let st_self loc ~self ~body = st_loc loc (ST_self { self; body })
let st_fix loc ~self ~body = st_loc loc (ST_fix { self; body })
let st_unroll loc ~term = st_loc loc (ST_unroll { term })
let st_pair loc ~left ~right = st_loc loc (ST_pair { left; right })
let st_both loc ~left ~right = st_loc loc (ST_both { left; right })
let st_bind loc ~bound ~value = st_loc loc (ST_bind { bound; value })
let st_semi loc ~left ~right = st_loc loc (ST_semi { left; right })
let st_annot loc ~value ~annot = st_loc loc (ST_annot { value; annot })
let st_string loc ~literal = st_loc loc (ST_string { literal })
let st_parens loc ~content = st_loc loc (ST_parens { content })
let st_braces loc ~content = st_loc loc (ST_braces { content })
