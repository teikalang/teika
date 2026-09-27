module Name = Name

type delimiter = Parens | Brackets

type term = CTerm of { term : term_syntax; loc : Location.t }

and term_syntax =
  | CT_hole
  | CT_var of { var : Name.t }
  | CT_unit of { delimiter : delimiter }
  | CT_apply of { funct : term; arg : term }
  | CT_pi of { bound : term; body : term }
  | CT_lambda of { bound : term; body : term }
  | CT_self of { bound : term; body : term }
  | CT_fix of { bound : term; body : term }
  | CT_unfold of { fix : term }
  | CT_fst of { pair : term }
  | CT_snd of { pair : term }
  | CT_pair of { left : term; right : term }
  | CT_inter of { left : term; right : term }
  | CT_union of { left : term; right : term }
  | CT_bind of { bound : term; value : term }
  | CT_semi of { left : term; right : term }
  | CT_annot of { value : term; annot : term }
  | CT_string of { literal : string }
  | CT_number of { literal : Z.t }
  | CT_parens of { content : term }
  | CT_brackets of { content : term }
  | CT_braces of { content : term }

let cterm loc term = CTerm { loc; term }
let ct_hole loc = cterm loc CT_hole
let ct_var loc ~var = cterm loc (CT_var { var })
let ct_unit loc ~delimiter = cterm loc (CT_unit { delimiter })
let ct_apply loc ~funct ~arg = cterm loc (CT_apply { funct; arg })
let ct_pi loc ~bound ~body = cterm loc (CT_pi { bound; body })
let ct_lambda loc ~bound ~body = cterm loc (CT_lambda { bound; body })
let ct_self loc ~bound ~body = cterm loc (CT_self { bound; body })
let ct_fix loc ~bound ~body = cterm loc (CT_fix { bound; body })
let ct_unfold loc ~fix = cterm loc (CT_unfold { fix })
let ct_fst loc ~pair = cterm loc (CT_fst { pair })
let ct_snd loc ~pair = cterm loc (CT_snd { pair })
let ct_pair loc ~left ~right = cterm loc (CT_pair { left; right })
let ct_inter loc ~left ~right = cterm loc (CT_inter { left; right })
let ct_union loc ~left ~right = cterm loc (CT_union { left; right })
let ct_bind loc ~bound ~value = cterm loc (CT_bind { bound; value })
let ct_semi loc ~left ~right = cterm loc (CT_semi { left; right })
let ct_annot loc ~value ~annot = cterm loc (CT_annot { value; annot })
let ct_string loc ~literal = cterm loc (CT_string { literal })
let ct_number loc ~literal = cterm loc (CT_number { literal })
let ct_parens loc ~content = cterm loc (CT_parens { content })
let ct_brackets loc ~content = cterm loc (CT_brackets { content })
let ct_braces loc ~content = cterm loc (CT_braces { content })
