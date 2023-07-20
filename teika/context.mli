open Ttree

type error = private
  (* metadata *)
  | CError_loc of { error : error; loc : Location.t [@opaque] }
  (* unify *)
  | CError_unify_bound_var_clash of { expected : Index.t; received : Index.t }
  | CError_unify_free_var_clash of { expected : Level.t; received : Level.t }
  | CError_unify_type_clash of {
      expected : ex_term;
      expected_norm : core term;
      received : ex_term;
      received_norm : core term;
    }
  (* TODO: lazy names for errors *)
  | CError_unify_var_occurs of { hole : hole; in_ : hole }
  | CError_unify_var_escape of { hole : hole; var : Level.t }
  (* typer *)
  | CError_typer_unknown_var of { name : Name.t }
  | Cerror_typer_not_a_forall of { type_ : ex_term }
  | CError_typer_pat_not_annotated of { pat : Ltree.pat }
  | CError_typer_pairs_not_implemented
[@@deriving show]

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
  val error_bound_var_clash :
    expected:Index.t -> received:Index.t -> 'a unify_context

  val error_free_var_clash :
    expected:Level.t -> received:Level.t -> 'a unify_context

  val error_type_clash :
    expected:_ term ->
    expected_norm:core term ->
    received:_ term ->
    received_norm:core term ->
    'a unify_context

  val error_var_occurs : hole:hole -> in_:hole -> 'a unify_context
  val error_var_escape : hole:hole -> var:Level.t -> 'a unify_context
end

module Typer_context : sig
  type 'a typer_context
  type 'a t = 'a typer_context

  (* monad *)
  val run : (unit -> 'a typer_context) -> ('a, error) result

  (* TODO: next_var must be bigger than type_of_types *)
  val test :
    next_var:Level.t ->
    vars:(Level.t * ex_term) Name.Map.t ->
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
  val error_pat_not_annotated : pat:Ltree.pat -> 'a typer_context
  val error_pairs_not_implemented : unit -> 'a typer_context
  val error_not_a_forall : type_:_ term -> 'a typer_context

  (* vars *)
  val lookup_var : name:Name.t -> (Level.t * ex_term) typer_context
  val with_expected_var : (unit -> 'a typer_context) -> 'a typer_context

  val with_received_var :
    name:Name.t ->
    type_:_ term ->
    (unit -> 'a typer_context) ->
    'a typer_context

  (* unify *)
  val with_unify_context : (unit -> 'a Unify_context.t) -> 'a typer_context

  (* locs *)
  val with_loc :
    loc:Location.t -> (unit -> 'a typer_context) -> 'a typer_context
end
