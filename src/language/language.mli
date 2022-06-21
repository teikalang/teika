type identifier = string
type number = string

type expr = LE of { loc : Location.t; desc : expr_desc }

and expr_desc =
  | LE_var of identifier
  | LE_number of number
  | LE_lambda of { param : pat; body : expr }
  | LE_apply of { lambda : expr; arg : expr }
  | LE_let of { bind : expr_bind; body : expr }
  | LE_record of expr_bind list
  | LE_annot of { value : expr; annot : annot }
  | LE_type of type_

and expr_bind = LE_bind of { loc : Location.t; bound : pat; value : expr }
and type_ = LT of { loc : Location.t; desc : type_desc }

and type_desc =
  | LT_var of identifier
  | LT_forall of { var : identifier; kind : kind; return : type_ }
  | LT_arrow of { param : type_; return : type_ }
  | LT_record of type_bind list

and type_bind =
  | LT_bind of { loc : Location.t; var : identifier; annot : annot }

and kind = LK of { loc : Location.t; desc : kind_desc }
and kind_desc = LK_type
(* | LK_arrow of { param : kind; return : kind } *)

and annot = LA_type of type_ | LA_kind of kind
and pat = LP of { loc : Location.t; desc : pat_desc }

and pat_desc =
  | LP_var of identifier
  | LP_record of pat list
  | LP_annot of { pat : pat; annot : annot }

val interpret_expr : Syntax.term -> expr
val interpret_type : Syntax.term -> type_
val interpret_pat : Syntax.term -> pat
