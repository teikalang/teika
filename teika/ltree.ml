type term = LTerm of { loc : Location.t; [@opaque] desc : term_desc }

and term_desc =
  | LT_var of { var : Name.t }
  | LT_forall of { param : annot; return : term }
  | LT_lambda of { param : annot; return : term }
  | LT_apply of { lambda : term; arg : term }
  | LT_exists of { left : annot; right : annot }
  | LT_pair of { left : bind; right : bind }
  | LT_unpair of { left : Name.t; right : Name.t; pair : term; return : term }
  | LT_let of { bound : bind; return : term }
  | LT_annot of { value : term; type_ : term }

and annot =
  | LAnnot of { loc : Location.t; [@opaque] var : Name.t; type_ : term }

and bind = LBind of { loc : Location.t; [@opaque] var : Name.t; value : term }
[@@deriving show]

(* term *)
let lterm loc desc = LTerm { loc; desc }
let lt_var loc ~var = lterm loc (LT_var { var })
let lt_forall loc ~param ~return = lterm loc (LT_forall { param; return })
let lt_lambda loc ~param ~return = lterm loc (LT_lambda { param; return })
let lt_apply loc ~lambda ~arg = lterm loc (LT_apply { lambda; arg })
let lt_exists loc ~left ~right = lterm loc (LT_exists { left; right })
let lt_pair loc ~left ~right = lterm loc (LT_pair { left; right })

let lt_unpair loc ~left ~right ~pair ~return =
  lterm loc (LT_unpair { left; right; pair; return })

let lt_let loc ~bound ~return = lterm loc (LT_let { bound; return })
let lt_annot loc ~value ~type_ = lterm loc (LT_annot { value; type_ })

(* annot *)
let lannot loc ~var ~type_ = LAnnot { loc; var; type_ }

(* bind *)
let lbind loc ~var ~value = LBind { loc; var; value }
