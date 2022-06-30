type expr = LE of { loc : Location.t; desc : expr_desc }

and expr_desc =
  | LE_var of { var : Name.t }
  | LE_lambda of { var : Name.t; param : type_; return : expr }
  | LE_forall of { var : Name.t; return : expr }
  | LE_apply of { funct : expr; arg : expr }
  | LE_pair of { left : bind; right : bind }
  | LE_exists of { var : Name.t; right : bind }
  | LE_type of { type_ : type_ }
  | LE_let of { bind : bind; return : expr }
  | LE_annot of { expr : expr; type_ : type_ }

and bind = LB of { loc : Location.t; var : Name.t; value : expr }
and type_ = LT of { loc : Location.t; desc : type_desc }

and type_desc =
  | LT_var of { var : Name.t }
  | LT_arrow of { param : type_; return : type_ }
  | LT_forall of { var : Name.t; return : type_ }
  | LT_pair of { left : type_; right : type_ }
  | LT_exists of { var : Name.t; right : type_ }

(* and kind = LK_type | LK_arrow of { param : kind; return : kind } *)

(* expr *)
let le loc desc = LE { loc; desc }
let le_var loc ~var = le loc (LE_var { var })

let le_lambda loc ~var ~param ~return =
  le loc (LE_lambda { var; param; return })

let le_forall loc ~var ~return = le loc (LE_forall { var; return })
let le_apply loc ~funct ~arg = le loc (LE_apply { funct; arg })
let le_pair loc ~left ~right = le loc (LE_pair { left; right })
let le_exists loc ~var ~right = le loc (LE_exists { var; right })
let le_type loc ~type_ = le loc (LE_type { type_ })
let le_let loc ~bind ~return = le loc (LE_let { bind; return })
let le_annot loc ~expr ~type_ = le loc (LE_annot { expr; type_ })

(* bind *)
let lb loc ~var ~value = LB { loc; var; value }

(* type *)
let lt loc desc = LT { loc; desc }
let lt_var loc ~var = lt loc (LT_var { var })
let lt_arrow loc ~param ~return = lt loc (LT_arrow { param; return })
let lt_forall loc ~var ~return = lt loc (LT_forall { var; return })
let lt_pair loc ~left ~right = lt loc (LT_pair { left; right })
let lt_exists loc ~var ~right = lt loc (LT_exists { var; right })
