open Ttree
open Terror

module Var_context : sig
  type 'a var_context
  type 'a t = 'a var_context

  (* monad *)
  val pure : 'a -> 'a var_context
  val ( let* ) : 'a var_context -> ('a -> 'b var_context) -> 'b var_context

  (* errors *)
  val error_unfold_found : term -> 'a var_context
  val error_annot_found : term -> 'a var_context
  val error_var_occurs : hole:term hole -> in_:term hole -> 'a var_context
  val error_var_escape : var:Level.t -> 'a var_context

  (* TODO: this should be removed *)
  val with_free_var : (unit -> 'a var_context) -> 'a var_context
  val level : unit -> Level.t var_context
end

module Unify_context : sig
  type 'a unify_context
  type 'a t = 'a unify_context

  (* monad *)
  val pure : 'a -> 'a unify_context

  val ( let* ) :
    'a unify_context -> ('a -> 'b unify_context) -> 'b unify_context

  (* error *)
  val error_unfold_found : expected:term -> received:term -> 'a unify_context
  val error_annot_found : expected:term -> received:term -> 'a unify_context

  val error_bound_var_clash :
    expected:Index.t -> received:Index.t -> 'a unify_context

  val error_free_var_clash :
    expected:Level.t -> received:Level.t -> 'a unify_context

  val error_type_clash : expected:term -> received:term -> 'a unify_context

  val error_string_clash :
    expected:string -> received:string -> 'a unify_context

  (* vars *)
  val with_free_vars : (unit -> 'a unify_context) -> 'a unify_context

  (* context *)
  val with_expected_var_context : (unit -> 'a Var_context.t) -> 'a unify_context
  val with_received_var_context : (unit -> 'a Var_context.t) -> 'a unify_context
end

module Typer_context : sig
  type 'a typer_context
  type 'a t = 'a typer_context

  (* monad *)
  val run : (unit -> 'a typer_context) -> ('a, error) result
  val pure : 'a -> 'a typer_context

  val ( let* ) :
    'a typer_context -> ('a -> 'b typer_context) -> 'b typer_context

  (* error *)
  val error_pairs_not_implemented : unit -> 'a typer_context

  val error_unknown_extension :
    extension:Name.t -> payload:Ltree.term -> 'a typer_context

  val error_unknown_native : native:string -> 'a typer_context

  (* TODO: this should be removed *)
  val level : unit -> Level.t typer_context
  val enter_level : (unit -> 'a typer_context) -> 'a typer_context

  (* vars *)

  (* TODO: names from both sides *)
  val with_free_vars :
    name:Name.t ->
    type_:term ->
    alias:term option ->
    (unit -> 'a typer_context) ->
    'a typer_context

  (* TODO: this is a hack *)
  val lookup_var : name:Name.t -> (term * term) typer_context

  (* locs *)
  val with_loc :
    loc:Location.t -> (unit -> 'a typer_context) -> 'a typer_context

  (* context *)
  val with_var_context : (unit -> 'a Var_context.t) -> 'a typer_context
  val with_unify_context : (unit -> 'a Unify_context.t) -> 'a typer_context
end
