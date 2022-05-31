open Utils
open Type

(* TODO: addapt tree to new Language tree *)
type term = {
  (* exposed env *)
  t_env : Env.t;
  t_loc : Location.t;
  t_type : type_;
  t_desc : term_desc;
}

and term_desc =
  | Term_var of Ident.t
  | Term_number of int
  (* TODO: what to put here as param? *)
  | Term_forall of { body : term }
  | Term_arrow of { param : term_pat; body : term }
  | Term_implicit_lambda of { body : term }
  | Term_explicit_lambda of { param : term_pat; body : term }
  | Term_apply of { lambda : term; arg : term }
  | Term_let of { bind : term_bind; body : term }
  | Term_record of term_bind list
  (* TODO: what to put here as content? *)
  | Term_signature
  | Term_asterisk
  | Term_annot of { value : term; type_ : term }

and term_bind =
  | TE_bind of {
      env : Env.t;
      loc : Location.t;
      names : (Name.t * type_) list;
      type_ : type_;
      bound : term_pat;
      value : term;
    }

and term_pat = {
  tp_env : Env.t;
  tp_loc : Location.t;
  tp_names : (Name.t * type_) list;
  tp_type : type_;
  tp_desc : term_pat_desc;
}

and term_pat_desc =
  | Term_pat_ident of Ident.t
  | Term_pat_struct of term_pat list
  | Term_pat_annot of { pat : term_pat; type_ : term }
