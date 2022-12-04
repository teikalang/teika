open Ttree

type error = private CError of { loc : Location.t; desc : error_desc }

and error_desc = private
  (* normalize *)
  | CError_normalize_unknown_hole_repr of { id : Uid.t }
  (* unify *)
  | CError_unify_var_clash of { expected : Offset.t; received : Offset.t }
  | CError_unify_type_clash of { expected : term; received : term }
  | CError_unify_pat_clash of { expected : pat; received : pat }
  | CError_unify_var_escape_scope of { var : Offset.t }
  (* invariant *)
  | CError_unify_unknown_hole of { id : Uid.t }
  | CError_unify_linked_hole of { id : Uid.t }
  | CError_unify_lower_to_higher of { id : Uid.t }
  (* typer *)
  | CError_typer_unknown_var of { var : Name.t }
  | Cerror_typer_not_a_forall of { type_ : term }
  | Cerror_typer_not_an_exists of { type_ : term }
  | CError_typer_pat_not_annotated of { pat : Ltree.pat }
  | CError_typer_pat_not_pair of { pat : Ltree.pat; expected : term }
  (* invariant *)
  | CError_typer_term_var_not_annotated of { var : Offset.t }
  | CError_typer_term_hole_not_annotated of { hole : Uid.t }
  | CError_typer_pat_var_not_annotated of { var : Name.t }
[@@deriving show]

module Normalize_context : sig
  type var_info = Subst of { to_ : term } | Bound of { base : Offset.t }
  type 'a normalize_context
  type 'a t = 'a normalize_context

  (* monad *)
  val test :
    loc:Warnings.loc ->
    vars:var_info list ->
    holes:hole array ->
    next_hole_id:Uid.t ->
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
  val repr_var : var:Offset.t -> term normalize_context
  val with_var : (unit -> 'a normalize_context) -> 'a normalize_context

  val elim_var :
    to_:term -> (unit -> 'a normalize_context) -> 'a normalize_context

  (* holes *)
  val repr_hole : id:Uid.t -> term normalize_context

  (* offset *)
  val with_offset :
    offset:Offset.t -> (unit -> 'a normalize_context) -> 'a normalize_context
end

(* TODO: this is bad *)
module Unify_context (Normalize : sig
  val normalize_term : term -> term Normalize_context.t
end) : sig
  type 'a unify_context
  type 'a t = 'a unify_context

  (* monad *)
  val test :
    loc:Warnings.loc ->
    holes:hole array ->
    next_hole_id:Uid.t ->
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

  val error_type_clash : expected:term -> received:term -> 'a unify_context
  val error_pat_clash : expected:pat -> received:pat -> 'a unify_context
  val error_var_escape_scope : var:Offset.t -> 'a unify_context

  (* normalize *)
  val normalize_term : term -> term unify_context

  (* offset *)
  val expected_offset : unit -> Offset.t unify_context
  val received_offset : unit -> Offset.t unify_context

  val with_expected_offset :
    offset:Offset.t -> (unit -> 'a unify_context) -> 'a unify_context

  val with_received_offset :
    offset:Offset.t -> (unit -> 'a unify_context) -> 'a unify_context

  (* holes *)
  val repr_hole : id:Uid.t -> hole unify_context
  val link_hole : id:Uid.t -> to_:term -> offset:Offset.t -> unit unify_context
  val lower_hole : id:Uid.t -> diff:Offset.t -> unit unify_context
end

module Typer_context (Normalize : sig
  val normalize_term : term -> term Normalize_context.t
end) (Unify : sig
  val unify_term :
    expected:term -> received:term -> unit Unify_context(Normalize).t
end) : sig
  type 'a typer_context
  type 'a t = 'a typer_context

  (* monad *)
  val run : loc:Location.t -> (unit -> 'a typer_context) -> ('a, error) result

  val test :
    loc:Warnings.loc ->
    holes:hole array ->
    next_hole_id:Uid.t ->
    type_of_types:Level.t ->
    level:Level.t ->
    names:(Level.t * term) Name.Tbl.t ->
    (unit -> 'a typer_context) ->
    ('a, error) result

  val return : 'a -> 'a typer_context
  val bind : 'a typer_context -> ('a -> 'b typer_context) -> 'b typer_context

  val ( let* ) :
    'a typer_context -> ('a -> 'b typer_context) -> 'b typer_context

  val ( let+ ) : 'a typer_context -> ('a -> 'b) -> 'b typer_context

  (* errors *)
  val error_pat_not_annotated : pat:Ltree.pat -> 'a typer_context
  val error_pat_not_pair : pat:Ltree.pat -> expected:term -> 'a typer_context
  val error_term_var_not_annotated : var:Offset.t -> 'a typer_context
  val error_term_hole_not_annotated : hole:Uid.t -> 'a typer_context
  val error_pat_var_not_annotated : var:Name.t -> 'a typer_context

  (* vars *)
  val instance : var:Name.t -> (Offset.t * term) typer_context

  val with_binder :
    var:Name.t -> type_:term -> (unit -> 'a typer_context) -> 'a typer_context

  (* unify *)
  val unify_term : expected:term -> received:term -> unit typer_context

  (* locs *)
  val with_tt_loc :
    loc:Location.t -> (unit -> term typer_context) -> term typer_context

  val with_tp_loc :
    loc:Location.t ->
    ((pat -> (pat -> 'k typer_context) -> 'k typer_context) -> 'k typer_context) ->
    'k typer_context

  (* ttree *)
  val tt_type : unit -> term typer_context
  val tt_var : annot:term -> offset:Offset.t -> term typer_context
  val tt_forall : param:pat -> return:term -> term typer_context
  val tt_lambda : param:pat -> return:term -> term typer_context
  val tt_apply : lambda:term -> arg:term -> term typer_context
  val tt_exists : left:annot -> right:annot -> term typer_context
  val tt_pair : left:bind -> right:bind -> term typer_context
  val tt_let : bound:bind -> return:term -> term typer_context
  val tt_annot : term:term -> annot:term -> term typer_context
  val tp_var : annot:term -> var:Name.t -> pat typer_context
  val tp_pair : left:pat -> right:pat -> pat typer_context
  val tp_annot : pat:pat -> annot:term -> pat typer_context
  val tannot : loc:Location.t -> pat:pat -> annot:term -> annot typer_context
  val tbind : loc:Location.t -> pat:pat -> value:term -> bind typer_context

  (* utils *)
  val split_forall : term -> (pat * term) typer_context
  val split_exists : term -> (annot * annot) typer_context
end
