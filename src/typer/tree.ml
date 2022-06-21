open Utils

type expr =
  | TE of {
      (* exposed env *)
      env : Env.t;
      loc : Location.t;
      type_ : Type.t;
      desc : expr_desc;
    }

and expr_desc =
  | TE_var of Ident.t
  | TE_number of int
  | TE_lambda of { param : pat; body : expr }
  | TE_apply of { lambda : expr; arg : expr }
  | TE_let of { bind : expr_bind; return : expr }
  | TE_record of expr_bind list
  | TE_annot of { value : expr; annot : annot }
  | TE_type of type_

and expr_bind =
  | TE_bind of {
      (* exposed env *)
      env : Env.t;
      loc : Location.t;
      names : (Name.t * Type.t) list;
      type_ : Type.t;
      bound : pat;
      value : expr;
    }

and type_ =
  | TT of {
      (* exposed env *)
      env : Env.t;
      loc : Location.t;
      type_ : Type.t;
      desc : type_desc;
    }

and type_desc =
  | TT_var of Ident.t
  | TT_forall of { var : Ident.t; kind : kind; return : type_ }
  | TT_arrow of { param : type_; return : type_ }
  | TT_record of type_bind list

and type_bind =
  | TT_bind of {
      (* exposed env *)
      env : Env.t;
      loc : Location.t;
      type_ : Type.t;
      var : Ident.t;
      annot : annot;
    }

and kind = TK of { loc : Location.t; desc : kind_desc }
and kind_desc = TK_type
and annot = TA_type of type_ | TA_kind of kind

and pat =
  | TP of {
      env : Env.t;
      loc : Location.t;
      names : (Name.t * type_) list;
      type_ : Type.t;
      desc : pat_desc;
    }

and pat_desc =
  | TP_var of Ident.t
  | TP_record of pat list
  | TP_annot of { pat : pat; annot : annot }
