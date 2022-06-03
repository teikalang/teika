open Utils
open Type

(* TODO: addapt tree to new Language tree *)
type expr =
  | TE of {
      (* exposed env *)
      env : Env.t;
      loc : Location.t;
      type_ : type_;
      desc : expr_desc;
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
  | TE_annot of { value : expr; annot : annot }

and term_bind =
  | TE_bind of {
      env : Env.t;
      loc : Location.t;
      names : (Name.t * type_) list;
      type_ : type_;
      bound : pat;
      value : expr;
    }

and pat =
  | TP of {
      env : Env.t;
      loc : Location.t;
      names : (Name.t * type_) list;
      type_ : type_;
      desc : pat_desc;
    }

and pat_desc =
  | TP_var of Ident.t
  | TP_record of pat list
  | TP_annot of { pat : pat; annot : annot }

and annot = TA_type of expr | TA_kind of kind
and kind = TK of { loc : Location.t; desc : kind_desc }
and kind_desc = TK_asterisk | TK_arrow of { param : kind; body : kind }
