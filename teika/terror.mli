open Utils
open Syntax
open Ttree

type error =
  (* metadata *)
  | TError_loc of { error : error; loc : Location.t [@opaque] }
  (* equal *)
  | TError_var_clash of { left : var; right : var }
  | TError_type_clash of { left : term; right : term }
  | TError_string_clash of { left : string; right : string }
  (* TODO: infer *)
  (* typer *)
  | TError_unknown_var of { name : Name.t }
  | TError_not_a_forall of { type_ : term }
  | TError_extensions_not_implemented
  | TError_pairs_not_implemented
  | TError_erasable_not_implemented
  | TError_unknown_extension of { extension : Name.t; payload : Ltree.term }
  (* TODO: native should not be a string *)
  | TError_unknown_native of { native : string }
  | TError_missing_annotation

type t = error [@@deriving show]

exception TError of { error : error }

(* TODO: error_loc *)
val error_var_clash : left:var -> right:var -> 'a
val error_type_clash : left:term -> right:term -> 'a
val error_string_clash : left:string -> right:string -> 'a
val error_unknown_var : name:Name.t -> 'a
val error_not_a_forall : type_:term -> 'a
val error_extensions_not_implemented : unit -> 'a
val error_pairs_not_implemented : unit -> 'a
val error_erasable_not_implemented : unit -> 'a
val error_unknown_extension : extension:Name.t -> payload:Ltree.term -> 'a
val error_unknown_native : native:string -> 'a
val error_missing_annotation : unit -> 'a
