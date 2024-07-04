open Utils
open Ttree

type error =
  (* metadata *)
  | TError_loc of { error : error; loc : Location.t [@opaque] }
  (* equal *)
  | TError_type_clash of { left : term; right : term }
  (* TODO: infer *)
  (* typer *)
  | TError_unknown_var of { name : Name.t }
  | TError_not_a_forall of { type_ : term }
  | TError_hoist_not_implemented
  | TError_extensions_not_implemented
  | TError_pairs_not_implemented
  (* TODO: native should not be a string *)
  | TError_unknown_native of { native : string }
  | TError_missing_annotation
  (* elaborate *)
  | TError_invalid_notation

type t = error [@@deriving show]

exception TError of { error : error }

(* TODO: error_loc *)
val error_type_clash : left:term -> right:term -> 'a
val error_unknown_var : name:Name.t -> 'a
val error_not_a_forall : type_:term -> 'a
val error_hoist_not_implemented : unit -> 'a
val error_extensions_not_implemented : unit -> 'a
val error_pairs_not_implemented : unit -> 'a
val error_unknown_native : native:string -> 'a
val error_missing_annotation : unit -> 'a
val error_invalid_notation : unit -> 'a
