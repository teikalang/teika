open Syntax
open Ttree

(* TODO: too much work to add errors,
   adding here and context is bad*)
type error =
  (* TODO: why track nested locations?
         Probably because things like macros exists *)
  | TError_loc of { error : error; loc : Location.t [@opaque] }
  (* misc *)
  | TError_misc_annot_found of { term : term }
  (* TODO: lazy names for errors *)
  | TError_misc_var_escape of { var : Level.t }
  (* unify *)
  | TError_unify_annot_found of { expected : term; received : term }
  | TError_unify_bound_var_clash of { expected : Index.t; received : Index.t }
  | TError_unify_free_var_clash of { expected : Level.t; received : Level.t }
  | TError_unify_type_clash of { expected : term; received : term }
  | TError_unify_string_clash of { expected : string; received : string }
  (* typer *)
  | TError_typer_unknown_var of { name : Name.t }
  | TError_typer_not_a_forall of { type_ : term }
  | TError_typer_pairs_not_implemented
  | TError_typer_erasable_not_implemented
  | TError_typer_unknown_extension of {
      extension : Name.t;
      payload : Ltree.term;
    }
  | TError_typer_unknown_native of { native : string }
  | TError_typer_missing_annotation

and t = error [@@deriving show { with_path = false }]
