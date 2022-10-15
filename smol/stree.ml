type term = ST of { loc : Location.t; [@opaque] desc : term_desc }

and term_desc =
  | ST_type
  | ST_var of { var : Name.t }
  | ST_arrow of { param : term; return : term }
  | ST_lambda of { param : term; return : term }
  | ST_apply of { lambda : term; arg : term }
  | ST_pair of { left : term; right : term }
  | ST_bind of { bound : term; value : term }
  | ST_semi of { left : term; right : term }
  | ST_annot of { value : term; type_ : term }
[@@deriving show { with_path = false }]

let st loc desc = ST { loc; desc }
let st_type loc = st loc ST_type
let st_var loc ~var = st loc (ST_var { var })
let st_arrow loc ~param ~return = st loc (ST_arrow { param; return })
let st_lambda loc ~param ~return = st loc (ST_lambda { param; return })
let st_apply loc ~lambda ~arg = st loc (ST_apply { lambda; arg })
let st_pair loc ~left ~right = st loc (ST_pair { left; right })
let st_bind loc ~bound ~value = st loc (ST_bind { bound; value })
let st_semi loc ~left ~right = st loc (ST_semi { left; right })
let st_annot loc ~value ~type_ = st loc (ST_annot { value; type_ })
