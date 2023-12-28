open Syntax
open Ttree
open Terror

module Unify_context : sig
  type 'a unify_context
  type 'a t = 'a unify_context

  (* monad *)
  val pure : 'a -> 'a unify_context

  val ( let* ) :
    'a unify_context -> ('a -> 'b unify_context) -> 'b unify_context

  (* error *)
  val error_annot_found : expected:term -> received:term -> 'a unify_context

  val error_bound_var_clash :
    expected:Index.t -> received:Index.t -> 'a unify_context

  val error_free_var_clash :
    expected:Level.t -> received:Level.t -> 'a unify_context

  val error_type_clash : expected:term -> received:term -> 'a unify_context

  val error_string_clash :
    expected:string -> received:string -> 'a unify_context

  (* vars *)
  val find_free_var_alias : var:Level.t -> term option unify_context
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
  val error_not_a_forall : type_:term -> 'a typer_context
  val error_pairs_not_implemented : unit -> 'a typer_context
  val error_erasable_not_implemented : unit -> 'a typer_context

  val error_unknown_extension :
    extension:Name.t -> payload:Ltree.term -> 'a typer_context

  val error_unknown_native : native:string -> 'a typer_context
  val error_missing_annotation : unit -> 'a typer_context

  (* TODO: this should be removed *)
  val level : unit -> Level.t typer_context

  (* vars *)
  val find_free_var_alias : var:Level.t -> term option typer_context

  (* TODO: names from both sides *)
  val with_free_vars :
    name:Name.t ->
    type_:term ->
    alias:term option ->
    (unit -> 'a typer_context) ->
    'a typer_context

  val lookup_var : name:Name.t -> (Level.t * term) typer_context

  (* locs *)
  val with_loc :
    loc:Location.t -> (unit -> 'a typer_context) -> 'a typer_context

  (* tools *)
  val pp_term : unit -> (Format.formatter -> term -> unit) typer_context
  val pp_error : unit -> (Format.formatter -> error -> unit) typer_context

  (* context *)
  val with_unify_context : (unit -> 'a Unify_context.t) -> 'a typer_context
end
