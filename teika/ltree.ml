type term =
  | LT_var of { var : Name.t }
  | LT_extension of { extension : Name.t; payload : term }
  | LT_forall of { param : pat; return : term }
  | LT_lambda of { param : pat; return : term }
  | LT_apply of { lambda : term; arg : term }
  | LT_exists of { left : annot; right : annot }
  | LT_pair of { left : bind; right : bind }
  | LT_let of { bound : bind; return : term }
  | LT_annot of { term : term; annot : term }
  | LT_string of { literal : string }
  | LT_loc of { term : term; loc : Location.t [@opaque] }

and pat =
  | LP_var of { var : Name.t }
  | LP_pair of { left : pat; right : pat }
  | LP_annot of { pat : pat; annot : term }
  | LP_loc of { pat : pat; loc : Location.t [@opaque] }

and annot = LAnnot of { loc : Location.t; [@opaque] pat : pat; annot : term }

and bind = LBind of { loc : Location.t; [@opaque] pat : pat; value : term }
[@@deriving show { with_path = false }]
