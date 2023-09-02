open Ttree
open Terror

type var_info = Free

(* TODO: this is bad *)
module Unify_context : sig
  type 'a unify_context
  type 'a t = 'a unify_context

  (* monad *)
  val test :
    expected_vars:var_info list ->
    received_vars:var_info list ->
    (unit -> 'a unify_context) ->
    ('a, error) result

  val return : 'a -> 'a unify_context
  val bind : 'a unify_context -> ('a -> 'b unify_context) -> 'b unify_context

  val ( let* ) :
    'a unify_context -> ('a -> 'b unify_context) -> 'b unify_context

  val ( let+ ) : 'a unify_context -> ('a -> 'b) -> 'b unify_context

  (* errors *)
  val error_subst_found : expected:term -> received:term -> 'a unify_context
  val error_annot_found : expected:term -> received:term -> 'a unify_context

  val error_bound_var_clash :
    expected:Index.t -> received:Index.t -> 'a unify_context

  val error_free_var_clash :
    expected:Level.t -> received:Level.t -> 'a unify_context

  val error_type_clash : expected:term -> received:term -> 'a unify_context
  val error_var_occurs : hole:term hole -> in_:term hole -> 'a unify_context

  val error_string_clash :
    expected:string -> received:string -> 'a unify_context
end

module Typer_context : sig
  type 'a typer_context
  type 'a t = 'a typer_context

  (* monad *)
  val run : (unit -> 'a typer_context) -> ('a, error) result

  (* TODO: next_var must be bigger than type_of_types *)
  val test :
    level:Level.t ->
    vars:(Level.t * term * term option) Name.Map.t ->
    expected_vars:var_info list ->
    received_vars:var_info list ->
    (unit -> 'a typer_context) ->
    ('a, error) result

  val return : 'a -> 'a typer_context
  val bind : 'a typer_context -> ('a -> 'b typer_context) -> 'b typer_context

  val ( let* ) :
    'a typer_context -> ('a -> 'b typer_context) -> 'b typer_context

  val ( let+ ) : 'a typer_context -> ('a -> 'b) -> 'b typer_context

  (* errors *)
  val error_pairs_not_implemented : unit -> 'a typer_context
  val error_not_a_forall : type_:term -> 'a typer_context
  val error_var_escape : var:Level.t -> 'a typer_context

  val error_typer_unknown_extension :
    extension:Name.t -> payload:Ltree.term -> 'a typer_context

  val error_typer_unknown_native : native:string -> 'a typer_context

  (* level *)
  val level : unit -> Level.t typer_context
  val enter_level : (unit -> 'a typer_context) -> 'a typer_context

  (* vars *)
  val lookup_var : name:Name.t -> (Level.t * term * term option) typer_context
  val with_expected_var : (unit -> 'a typer_context) -> 'a typer_context

  val with_received_var :
    name:Name.t ->
    type_:term ->
    alias:term option ->
    (unit -> 'a typer_context) ->
    'a typer_context

  (* unify *)
  val with_unify_context : (unit -> 'a Unify_context.t) -> 'a typer_context

  (* locs *)
  val with_loc :
    loc:Location.t -> (unit -> 'a typer_context) -> 'a typer_context
end
