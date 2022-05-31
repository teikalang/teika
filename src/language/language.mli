type identifier = string
type number = string

type expr = { le_loc : Location.t; le_desc : expr_desc }

and expr_desc =
  | LE_var of identifier
  | LE_number of number
  | LE_arrow of { param : pat; body : expr }
  | LE_lambda of { param : pat; body : expr }
  | LE_apply of { lambda : expr; arg : expr }
  | LE_let of { bind : le_bind; body : expr }
  | LE_record of le_bind list
  | LE_signature of pat list
  | LE_asterisk
  | LE_annot of { value : expr; type_ : expr }

and le_bind = LE_bind of { loc : Location.t; bound : pat; value : expr }
and pat = { lp_loc : Location.t; lp_desc : pat_desc }

and pat_desc =
  | LP_var of identifier
  | LP_record of pat list
  | LP_annot of { pat : pat; type_ : expr }

val interpret_expr : Syntax.term -> expr
val interpret_pat : Syntax.term -> pat
