open Utils
open Type

(* TODO: addapt tree to new Language tree *)
type expr = {
  (* exposed env *)
  te_env : Env.t;
  te_loc : Location.t;
  te_type : type_;
  te_desc : expr_desc;
}

and expr_desc =
  | TE_var of Ident.t
  | TE_number of int
  (* TODO: what to put here as param? *)
  | TE_forall of { body : expr }
  | TE_arrow of { param : pat; body : expr }
  | TE_implicit_lambda of { body : expr }
  | TE_explicit_lambda of { param : pat; body : expr }
  | TE_apply of { lambda : expr; arg : expr }
  | TE_let of { bind : term_bind; body : expr }
  | TE_record of term_bind list
  (* TODO: what to put here as content? *)
  | TE_signature
  | TE_asterisk
  | TE_annot of { value : expr; type_ : expr }

and term_bind =
  | TE_bind of {
      env : Env.t;
      loc : Location.t;
      names : (Name.t * type_) list;
      type_ : type_;
      bound : pat;
      value : expr;
    }

and pat = {
  tp_env : Env.t;
  tp_loc : Location.t;
  tp_names : (Name.t * type_) list;
  tp_type : type_;
  tp_desc : pat_desc;
}

and pat_desc =
  | TP_var of Ident.t
  | TP_record of pat list
  | TP_annot of { pat : pat; type_ : expr }
