type term = LT of { loc : Location.t; [@opaque] desc : term_desc }

and term_desc =
  | LT_var of { var : Name.t }
  | LT_literal of { literal : Literal.t }
  | LT_arrow of { var : Name.t; param : term; return : term }
  | LT_lambda of { var : Name.t; param : term; return : term }
  | LT_apply of { lambda : term; arg : term }
  | LT_sigma of { var : Name.t; left : term; right : term }
  | LT_pair of { var : Name.t; left : term; right : term; annot : term }
  | LT_unpair of { left : Name.t; right : Name.t; pair : term; return : term }
  | LT_let of { var : Name.t; value : term; return : term }
  | LT_annot of { value : term; type_ : term }
[@@deriving show { with_path = false }]

let lt loc desc = LT { loc; desc }
let lt_var loc ~var = lt loc (LT_var { var })
let lt_literal loc ~literal = lt loc (LT_literal { literal })
let lt_arrow loc ~var ~param ~return = lt loc (LT_arrow { var; param; return })

let lt_lambda loc ~var ~param ~return =
  lt loc (LT_lambda { var; param; return })

let lt_apply loc ~lambda ~arg = lt loc (LT_apply { lambda; arg })
let lt_sigma loc ~var ~left ~right = lt loc (LT_sigma { var; left; right })

let lt_pair loc ~var ~left ~right ~annot =
  lt loc (LT_pair { var; left; right; annot })

let lt_unpair loc ~left ~right ~pair ~return =
  lt loc (LT_unpair { left; right; pair; return })

let lt_let loc ~var ~value ~return = lt loc (LT_let { var; value; return })
let lt_annot loc ~value ~type_ = lt loc (LT_annot { value; type_ })
