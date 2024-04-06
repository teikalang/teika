open Utils
open Syntax
open Ttree

(* TODO: too much work to add errors,
   adding here and context is bad*)
type error =
  (* TODO: why track nested locations?
         Probably because things like macros exists *)
  | TError_loc of { error : error; loc : Location.t [@opaque] }
  (* equal *)
  | TError_var_clash of { left : var; right : var }
  | TError_type_clash of { left : term; right : term }
  | TError_string_clash of { left : string; right : string }
  (* typer *)
  | TError_unknown_var of { name : Name.t }
  | TError_not_a_forall of { type_ : term }
  | TError_extensions_not_implemented
  | TError_pairs_not_implemented
  | TError_erasable_not_implemented
  | TError_unknown_extension of { extension : Name.t; payload : Ltree.term }
  | TError_unknown_native of { native : string }
  | TError_missing_annotation

and t = error [@@deriving show { with_path = false }]

exception TError of { error : error }

let terror error = raise (TError { error })
let error_var_clash ~left ~right = terror @@ TError_var_clash { left; right }
let error_type_clash ~left ~right = terror @@ TError_type_clash { left; right }

let error_string_clash ~left ~right =
  terror @@ TError_string_clash { left; right }

let error_unknown_var ~name = terror @@ TError_unknown_var { name }
let error_not_a_forall ~type_ = terror @@ TError_not_a_forall { type_ }

let error_extensions_not_implemented () =
  terror @@ TError_extensions_not_implemented

let error_pairs_not_implemented () = terror @@ TError_pairs_not_implemented

let error_erasable_not_implemented () =
  terror @@ TError_erasable_not_implemented

let error_unknown_extension ~extension ~payload =
  terror @@ TError_unknown_extension { extension; payload }

let error_unknown_native ~native = terror @@ TError_unknown_native { native }
let error_missing_annotation () = terror @@ TError_missing_annotation
