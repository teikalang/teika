open Ttree

type error =
  (* metadata *)
  | TError_loc of { error : error; loc : Location.t [@opaque] }
  (* misc *)
  | TError_misc_subst_found of { term : term }
  | TError_misc_bound_var_found of { term : term }
  | TError_misc_unfold_found of { term : term }
  | TError_misc_annot_found of { term : term }
  (* TODO: lazy names for errors *)
  | TError_misc_var_occurs of { hole : term hole; in_ : term hole }
  | TError_misc_var_escape of { var : Level.t }
  (* unify *)
  | TError_unify_subst_found of { expected : term; received : term }
  | TError_unify_bound_var_found of { expected : term; received : term }
  | TError_unify_unfold_found of { expected : term; received : term }
  | TError_unify_annot_found of { expected : term; received : term }
  | TError_unify_bound_var_clash of { expected : Index.t; received : Index.t }
  | TError_unify_free_var_clash of { expected : Level.t; received : Level.t }
  | TError_unify_type_clash of { expected : term; received : term }
  | TError_unify_string_clash of { expected : string; received : string }
  (* typer *)
  | TError_typer_unknown_var of { name : Name.t }
  | TError_typer_not_a_forall of { type_ : term }
  | TError_typer_pairs_not_implemented
  | TError_typer_unknown_extension of {
      extension : Name.t;
      payload : Ltree.term;
    }
  | TError_typer_unknown_native of { native : string }

type t = error [@@deriving show]
