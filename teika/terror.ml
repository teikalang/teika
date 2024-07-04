open Utils
open Ttree

(* TODO: too much work to add errors,
   adding here and context is bad*)
type error =
  (* TODO: why track nested locations?
         Probably because things like macros exists *)
  | TError_loc of { error : error; loc : Location.t [@opaque] }
  (* equal *)
  | TError_type_clash of { left : term; right : term }
  (* typer *)
  | TError_unknown_var of { name : Name.t }
  | TError_not_a_forall of { type_ : term }
  | TError_hoist_not_implemented
  | TError_extensions_not_implemented
  | TError_pairs_not_implemented
  | TError_unknown_native of { native : string }
  | TError_missing_annotation
  (* elaborate *)
  | TError_invalid_notation

and t = error [@@deriving show { with_path = false }]

exception TError of { error : error }

let terror error = raise (TError { error })
let error_type_clash ~left ~right = terror @@ TError_type_clash { left; right }
let error_unknown_var ~name = terror @@ TError_unknown_var { name }
let error_not_a_forall ~type_ = terror @@ TError_not_a_forall { type_ }
let error_hoist_not_implemented () = terror @@ TError_hoist_not_implemented

let error_extensions_not_implemented () =
  terror @@ TError_extensions_not_implemented

let error_pairs_not_implemented () = terror @@ TError_pairs_not_implemented
let error_unknown_native ~native = terror @@ TError_unknown_native { native }
let error_missing_annotation () = terror @@ TError_missing_annotation
let error_invalid_notation () = terror @@ TError_invalid_notation
