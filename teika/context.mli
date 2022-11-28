open Ttree

type error = private CError of { loc : Location.t; desc : error_desc }

and error_desc = private
  (* unify *)
  | CError_unify_var_clash of { expected : Offset.t; received : Offset.t }
  | CError_unify_type_clash of { expected : term_desc; received : term_desc }
  (* typer *)
  | CError_typer_unknown_var of { var : Name.t }
  | Cerror_typer_not_a_forall of { type_ : type_ }
  | Cerror_typer_not_an_exists of { type_ : type_ }

module Normalize_context : sig
  type var_info = Subst of { to_ : term_desc } | Bound of { base : Offset.t }
  type 'a normalize_context
  type 'a t = 'a normalize_context

  (* monad *)
  val test :
    loc:Warnings.loc ->
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
  val repr_var : var:Offset.t -> term_desc normalize_context
  val with_var : (unit -> 'a normalize_context) -> 'a normalize_context

  val elim_var :
    to_:term_desc -> (unit -> 'a normalize_context) -> 'a normalize_context

  (* offset *)
  val with_offset :
    offset:Offset.t -> (unit -> 'a normalize_context) -> 'a normalize_context
end

(* TODO: this is bad *)
module Unify_context (Normalize : sig
  val normalize_term : term -> term Normalize_context.t
  val normalize_type : type_ -> type_ Normalize_context.t
end) : sig
  type 'a unify_context
  type 'a t = 'a unify_context

  (* monad *)
  val test :
    loc:Warnings.loc ->
    expected_offset:Offset.t ->
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

  val error_type_clash :
    expected:term_desc -> received:term_desc -> 'a unify_context

  (* normalize *)
  val normalize_term : term -> term unify_context
  val normalize_type : type_ -> type_ unify_context

  (* offset *)
  val repr_expected_var : var:Offset.t -> Offset.t unify_context
  val repr_received_var : var:Offset.t -> Offset.t unify_context

  val with_expected_offset :
    offset:Offset.t -> (unit -> 'a unify_context) -> 'a unify_context

  val with_received_offset :
    offset:Offset.t -> (unit -> 'a unify_context) -> 'a unify_context
end

module Typer_context (Normalize : sig
  val normalize_term : term -> term Normalize_context.t
  val normalize_type : type_ -> type_ Normalize_context.t
end) (Unify : sig
  val unify_type :
    expected:type_ -> received:type_ -> unit Unify_context(Normalize).t
end) : sig
  type 'a typer_context
  type 'a t = 'a typer_context

  (* monad *)
  val run : loc:Location.t -> (unit -> 'a typer_context) -> ('a, error) result

  val test :
    loc:Warnings.loc ->
    type_of_types:Level.t ->
    level:Level.t ->
    names:(Level.t * type_) Name.Tbl.t ->
    (unit -> 'a typer_context) ->
    ('a, error) result

  val return : 'a -> 'a typer_context
  val bind : 'a typer_context -> ('a -> 'b typer_context) -> 'b typer_context

  val ( let* ) :
    'a typer_context -> ('a -> 'b typer_context) -> 'b typer_context

  val ( let+ ) : 'a typer_context -> ('a -> 'b) -> 'b typer_context

  (* vars *)
  val instance : var:Name.t -> (Offset.t * type_) typer_context

  val with_binder :
    var:Name.t -> type_:type_ -> (unit -> 'a typer_context) -> 'a typer_context

  (* unify *)
  val unify_type : expected:type_ -> received:type_ -> unit typer_context

  (* locs *)
  val with_loc :
    loc:Location.t -> (unit -> 'a typer_context) -> 'a typer_context

  (* ttree *)
  val tt_type : unit -> type_ typer_context
  val tt_var : type_ -> offset:Offset.t -> term typer_context
  val tt_forall : param:annot -> return:type_ -> type_ typer_context
  val tt_lambda : type_ -> param:annot -> return:term -> term typer_context
  val tt_apply : type_ -> lambda:term -> arg:term -> term typer_context
  val tt_exists : left:annot -> right:annot -> type_ typer_context
  val tt_pair : type_ -> left:bind -> right:bind -> term typer_context

  val tt_unpair :
    type_ ->
    left:Name.t ->
    right:Name.t ->
    pair:term ->
    return:term ->
    term typer_context

  val tt_let : type_ -> bound:bind -> return:term -> term typer_context
  val tt_annot : value:term -> annot:type_ -> term typer_context
  val tannot : var:Name.t -> annot:type_ -> annot typer_context
  val tbind : var:Name.t -> value:term -> bind typer_context

  (* utils *)
  val term_of_type : type_ -> term typer_context
  val type_of_term : term -> type_ typer_context
  val split_forall : type_ -> (annot * type_) typer_context
  val split_exists : type_ -> (annot * annot) typer_context
end
