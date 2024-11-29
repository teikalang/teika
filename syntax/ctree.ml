open Utils

type term =
  (* TODO: printer location *)
  | CTerm of { term : term_syntax; loc : Location.t [@opaque] }

and term_syntax =
  | CT_var of { var : Name.t }
  | CT_extension of { extension : Name.t }
  | CT_forall of { param : term; body : term }
  | CT_lambda of { param : term; body : term }
  | CT_apply of { funct : term; arg : term }
  | CT_pair of { left : term; right : term }
  | CT_both of { left : term; right : term }
  | CT_bind of { bound : term; value : term }
  | CT_semi of { left : term; right : term }
  | CT_annot of { value : term; annot : term }
  | CT_string of { literal : string }
  | CT_number of { literal : Z.t [@printer Z.pp_print] }
  | CT_parens of { content : term }
  | CT_braces of { content : term }
[@@deriving show { with_path = false }]

let cterm loc term = CTerm { loc; term }
let ct_var loc ~var = cterm loc (CT_var { var })
let ct_extension loc ~extension = cterm loc (CT_extension { extension })
let ct_forall loc ~param ~body = cterm loc (CT_forall { param; body })
let ct_lambda loc ~param ~body = cterm loc (CT_lambda { param; body })
let ct_apply loc ~funct ~arg = cterm loc (CT_apply { funct; arg })
let ct_pair loc ~left ~right = cterm loc (CT_pair { left; right })
let ct_both loc ~left ~right = cterm loc (CT_both { left; right })
let ct_bind loc ~bound ~value = cterm loc (CT_bind { bound; value })
let ct_semi loc ~left ~right = cterm loc (CT_semi { left; right })
let ct_annot loc ~value ~annot = cterm loc (CT_annot { value; annot })
let ct_string loc ~literal = cterm loc (CT_string { literal })
let ct_number loc ~literal = cterm loc (CT_number { literal })
let ct_parens loc ~content = cterm loc (CT_parens { content })
let ct_braces loc ~content = cterm loc (CT_braces { content })
