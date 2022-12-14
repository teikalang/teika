open Ttree

type error = private
  (* metadata *)
  | CError_loc of { error : error; loc : Location.t [@opaque] }
  (* unify *)
  | CError_unify_var_clash of { expected : Offset.t; received : Offset.t }
  | CError_unify_type_clash of { expected : ex_term; received : ex_term }
  | CError_unify_pat_clash of { expected : ex_pat; received : ex_pat }
  | CError_unify_var_escape_scope of { var : Offset.t }
  (* typer *)
  | CError_typer_unknown_var of { name : Name.t }
  | Cerror_typer_not_a_forall of { type_ : ex_term }
  | CError_typer_pat_not_annotated of { pat : Ltree.pat }
  | CError_typer_pairs_not_implemented
  (* invariant *)
  | CError_typer_term_var_not_annotated of { var : Offset.t }
  | CError_typer_pat_var_not_annotated of { name : Name.t }
[@@deriving show]

type var_info = Subst of { to_ : ex_term } | Bound of { base : Offset.t }

module Normalize_context : sig
  type 'a normalize_context
  type 'a t = 'a normalize_context

  (* monad *)
  val test :
    vars:var_info list ->
    offset:Offset.t ->
    (unit -> 'a normalize_context) ->
    ('a, error) result

  val return : 'a -> 'a normalize_context

  val bind :
    'a normalize_context -> ('a -> 'b normalize_context) -> 'b normalize_context

  val ( let* ) :
    'a normalize_context -> ('a -> 'b normalize_context) -> 'b normalize_context

  val ( let+ ) : 'a normalize_context -> ('a -> 'b) -> 'b normalize_context

  (* vars *)
  val repr_var : var:Offset.t -> ex_term normalize_context
  val with_var : (unit -> 'a normalize_context) -> 'a normalize_context

  val elim_var :
    to_:ex_term -> (unit -> 'a normalize_context) -> 'a normalize_context

  (* offset *)
  val with_offset :
    offset:Offset.t -> (unit -> 'a normalize_context) -> 'a normalize_context
end

(* TODO: this is bad *)
module Unify_context : sig
  type 'a unify_context
  type 'a t = 'a unify_context

  (* monad *)
  val test :
    expected_vars:var_info list ->
    expected_offset:Offset.t ->
    received_vars:var_info list ->
    received_offset:Offset.t ->
    (unit -> 'a unify_context) ->
    ('a, error) result

  val return : 'a -> 'a unify_context
  val bind : 'a unify_context -> ('a -> 'b unify_context) -> 'b unify_context

  val ( let* ) :
    'a unify_context -> ('a -> 'b unify_context) -> 'b unify_context

  val ( let+ ) : 'a unify_context -> ('a -> 'b) -> 'b unify_context

  (* errors *)
  val error_var_clash :
    expected:Offset.t -> received:Offset.t -> 'a unify_context

  val error_type_clash : expected:_ term -> received:_ term -> 'a unify_context
  val error_pat_clash : expected:_ pat -> received:_ pat -> 'a unify_context
  val error_var_escape_scope : var:Offset.t -> 'a unify_context

  (* normalize *)
  val with_expected_normalize_context :
    (unit -> 'a Normalize_context.t) -> 'a unify_context

  val with_received_normalize_context :
    (unit -> 'a Normalize_context.t) -> 'a unify_context

  (* offset *)
  val expected_offset : unit -> Offset.t unify_context
  val received_offset : unit -> Offset.t unify_context

  val with_expected_offset :
    offset:Offset.t -> (unit -> 'a unify_context) -> 'a unify_context

  val with_received_offset :
    offset:Offset.t -> (unit -> 'a unify_context) -> 'a unify_context
end

module Typer_context : sig
  type 'a typer_context
  type 'a t = 'a typer_context

  (* monad *)
  val run : (unit -> 'a typer_context) -> ('a, error) result

  val test :
    type_of_types:Level.t ->
    level:Level.t ->
    names:(Level.t * ex_term) Name.Map.t ->
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
  val error_term_var_not_annotated : var:Offset.t -> 'a typer_context
  val error_pat_var_not_annotated : name:Name.t -> 'a typer_context
  val error_pairs_not_implemented : unit -> 'a typer_context
  val error_not_a_forall : type_:_ term -> 'a typer_context

  (* vars *)
  val instance : name:Name.t -> (Offset.t * ex_term) typer_context

  val with_binder :
    name:Name.t ->
    type_:_ term ->
    (unit -> 'a typer_context) ->
    'a typer_context

  (* normalize *)
  val with_received_normalize_context :
    (unit -> 'a Normalize_context.t) -> 'a typer_context

  (* unify *)
  val with_unify_context : (unit -> 'a Unify_context.t) -> 'a typer_context

  (* locs *)
  val with_loc :
    loc:Location.t -> (unit -> 'a typer_context) -> 'a typer_context

  (* ttree *)
  val tt_type : unit -> core term typer_context
end
