type term = LTerm of { loc : Location.t; [@opaque] desc : term_desc }

and term_desc =
  | LT_var of { var : Name.t }
  | LT_forall of { param : pat; return : term }
  | LT_lambda of { param : pat; return : term }
  | LT_apply of { lambda : term; arg : term }
  | LT_exists of { left : annot; right : annot }
  | LT_pair of { left : bind; right : bind }
  | LT_let of { bound : bind; return : term }
  | LT_annot of { value : term; annot : term }

and pat = LPat of { loc : Location.t; [@opaque] desc : pat_desc }

and pat_desc =
  (* x *)
  | LP_var of { var : Name.t }
  (* (x, y) *)
  | LP_pair of { left : pat; right : pat }
  (* (p : T) *)
  | LP_annot of { pat : pat; annot : term }

and annot = LAnnot of { loc : Location.t; [@opaque] pat : pat; annot : term }

and bind = LBind of { loc : Location.t; [@opaque] pat : pat; value : term }
[@@deriving show]

(* term *)
let lterm loc desc = LTerm { loc; desc }
let lt_var loc ~var = lterm loc (LT_var { var })
let lt_forall loc ~param ~return = lterm loc (LT_forall { param; return })
let lt_lambda loc ~param ~return = lterm loc (LT_lambda { param; return })
let lt_apply loc ~lambda ~arg = lterm loc (LT_apply { lambda; arg })
let lt_exists loc ~left ~right = lterm loc (LT_exists { left; right })
let lt_pair loc ~left ~right = lterm loc (LT_pair { left; right })
let lt_let loc ~bound ~return = lterm loc (LT_let { bound; return })
let lt_annot loc ~value ~annot = lterm loc (LT_annot { value; annot })

(* pattern *)
let lpat loc desc = LPat { loc; desc }
let lp_var loc ~var = lpat loc (LP_var { var })
let lp_pair loc ~left ~right = lpat loc (LP_pair { left; right })
let lp_annot loc ~pat ~annot = lpat loc (LP_annot { pat; annot })

(* annot *)
let lannot loc ~pat ~annot = LAnnot { loc; pat; annot }

(* bind *)
let lbind loc ~pat ~value = LBind { loc; pat; value }
