(* TODO: loc first vs loc last *)
type expr = { expr_loc : Location.t; expr_desc : expr_desc }

and expr_desc =
  | Expr_ident of Ident.t
  | Expr_number of int
  | Expr_lambda of { param : pat; body : expr }
  | Expr_apply of { lambda : expr; arg : expr }
  | Expr_bind of { bound : pat; value : expr; body : expr }
  | Expr_annot of { value : expr; type_ : type_ }

and pat = { pat_loc : Location.t; pat_desc : pat_desc }

and pat_desc =
  | Pat_ident of Ident.t
  | Pat_annot of { pat : pat; constraint_ : type_ }

and type_ = { type_loc : Location.t; type_desc : type_desc }

and type_desc =
  | Type_int
  | Type_ident of Ident.t
  (* TODO: likely this should be fused *)
  | Type_implicit_lambda of { body : type_ }
  | Type_explicit_lambda of {
      (* TODO: param = pat *) param : type_;
      body : type_;
    }
