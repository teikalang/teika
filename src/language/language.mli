type identifier = string
type number = string

type expr = LE of { loc : Location.t; desc : expr_desc }

and expr_desc =
  | LE_var of identifier
  | LE_number of number
  | LE_arrow of { implicit : bool; param : pat; body : expr }
  | LE_lambda of { implicit : bool; param : pat; body : expr }
  | LE_apply of { lambda : expr; arg : expr }
  | LE_let of { bind : le_bind; body : expr }
  | LE_record of le_bind list
  | LE_signature of pat list
  | LE_annot of { value : expr; annot : annot }

and le_bind = LE_bind of { loc : Location.t; bound : pat; value : expr }
and pat = LP of { loc : Location.t; desc : pat_desc }

and pat_desc =
  | LP_var of identifier
  | LP_record of pat list
  | LP_annot of { pat : pat; annot : annot }

and annot = LA_type of expr | LA_kind of kind
and kind = LK of { loc : Location.t; desc : kind_desc }
and kind_desc = LK_asterisk | LK_arrow of { param : kind; body : kind }

val interpret_expr : Syntax.term -> expr
val interpret_pat : Syntax.term -> pat
