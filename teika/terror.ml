open Ttree
open Tprinter

type error =
  (* TODO: why track nested locations?
         Probably because things like macros exists *)
  | TError_loc of { error : error; loc : Location.t [@opaque] }
  (* unify *)
  | TError_unify_subst_found of { expected : term; received : term }
  | TError_unify_annot_found of { expected : term; received : term }
  | TError_unify_bound_var_clash of { expected : Index.t; received : Index.t }
  | TError_unify_free_var_clash of { expected : Level.t; received : Level.t }
  | TError_unify_type_clash of { expected : term; received : term }
    (* TODO: lazy names for errors *)
  | TError_unify_var_occurs of {
      hole : term hole; [@printer Tprinter.pp_term_hole]
      in_ : term hole; [@printer Tprinter.pp_term_hole]
    }
  | TError_unify_string_clash of { expected : string; received : string }
  (* typer *)
  | TError_typer_unknown_var of { name : Name.t }
  | TError_typer_not_a_forall of { type_ : term }
  | TError_typer_pairs_not_implemented
  | TError_typer_var_escape of { var : Level.t }
  | TError_typer_unknown_extension of {
      extension : Name.t;
      payload : Ltree.term;
    }
  | TError_typer_unknown_native of { native : string }

and t = error [@@deriving show { with_path = false }]
