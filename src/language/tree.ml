type identifier = string
type number = string

type expr = LE of { loc : Location.t; desc : expr_desc }

and expr_desc =
  (* TODO: *)
  (* _ *)
  (* | LE_hole *)
  (* x *)
  | LE_var of identifier
  (* 123 *)
  | LE_number of number
  (* pat -> expr *)
  | LE_arrow of { implicit : bool; param : pat; body : expr }
  (* pat => expr *)
  | LE_lambda of { implicit : bool; param : pat; body : expr }
  (* expr expr *)
  | LE_apply of { lambda : expr; arg : expr }
  (* pat = expr; expr *)
  | LE_let of { bind : le_bind; body : expr }
  (* { pat = expr; } *)
  | LE_record of le_bind list
  (* TODO: should LE_signature allow any pattern? *)
  (* { pat; } *)
  | LE_signature of pat list
  (* expr : type *)
  | LE_annot of { value : expr; annot : annot }

(* TODO: bind directly on parser *)
and le_bind = LE_bind of { loc : Location.t; bound : pat; value : expr }

(* TODO: can pattern be unified back on expr? *)
and pat = LP of { loc : Location.t; desc : pat_desc }

and pat_desc =
  (* TODO: any*)
  (* _ *)
  (* | LP_any *)
  (* x *)
  | LP_var of identifier
  | LP_record of pat list
  (* pat : type *)
  | LP_annot of { pat : pat; annot : annot }

(* TODO: is this syntatic difference really needed? *)
and annot = LA_type of expr | LA_kind of kind
and kind = LK of { loc : Location.t; desc : kind_desc }

and kind_desc =
  (* * *)
  | LK_asterisk
  (* kind -> kind *)
  | LK_arrow of { param : kind; body : kind }
