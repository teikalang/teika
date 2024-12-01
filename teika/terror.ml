open Utils
open Ttree

(* TODO: too much work to add errors,
   adding here and context is bad*)
type error =
  (* TODO: why track nested locations?
         Probably because things like macros exists *)
  | TError_loc of { error : error; loc : Location.t [@opaque] }
  (* equal *)
  | TError_type_clash
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

let show_hum_error error =
  match error with
  | TError_loc { error = _; loc = _ } -> show_error error
  | TError_type_clash -> "type clash"
  | TError_unknown_var { name } ->
      Format.asprintf "unknown var: %a" Name.pp name
  | TError_not_a_forall { type_ } ->
      Format.asprintf "not a forall: %a" pp_term type_
  | TError_hoist_not_implemented -> "hoist not implemented"
  | TError_extensions_not_implemented -> "extensions not implemented"
  | TError_pairs_not_implemented -> "pairs not implemented"
  | TError_unknown_native { native } ->
      Format.asprintf "unknown native: %s" native
  | TError_missing_annotation -> "missing annotation"
  | TError_invalid_notation -> "invalid notation"

let () =
  Printexc.register_printer @@ function
  | TError { error } -> Some (show_hum_error error)
  | _ -> None

let terror error = raise (TError { error })
let error_type_clash () = terror @@ TError_type_clash
let error_unknown_var ~name = terror @@ TError_unknown_var { name }
let error_not_a_forall ~type_ = terror @@ TError_not_a_forall { type_ }
let error_hoist_not_implemented () = terror @@ TError_hoist_not_implemented

let error_extensions_not_implemented () =
  terror @@ TError_extensions_not_implemented

let error_pairs_not_implemented () = terror @@ TError_pairs_not_implemented
let error_unknown_native ~native = terror @@ TError_unknown_native { native }
let error_missing_annotation () = terror @@ TError_missing_annotation
let error_invalid_notation () = terror @@ TError_invalid_notation
