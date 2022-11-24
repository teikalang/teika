open Ttree

type error = private CError of { loc : Location.t; desc : error_desc }

and error_desc = private
  (* unify *)
  | CError_unify_var_clash of { expected : Offset.t; received : Offset.t }
  | CError_unify_type_clash of { expected : term_desc; received : term_desc }
  (* typer *)
  | CError_typer_unknown_var of { var : Name.t }
  | CError_typer_term_not_a_type of { term : term }
  | Cerror_typer_not_a_forall of { type_ : type_ }
  | Cerror_typer_not_an_exists of { type_ : type_ }

module Instance_context : sig
  type 'a instance_context
  type 'a t = 'a instance_context

  (* monad *)
  val test :
    loc:Warnings.loc ->
    offset:Offset.t ->
    (unit -> 'a instance_context) ->
    ('a, error) result

  val return : 'a -> 'a instance_context

  val bind :
    'a instance_context -> ('a -> 'b instance_context) -> 'b instance_context

  val ( let* ) :
    'a instance_context -> ('a -> 'b instance_context) -> 'b instance_context

  val ( let+ ) : 'a instance_context -> ('a -> 'b) -> 'b instance_context

  (* monad *)
  val offset : unit -> Offset.t instance_context
end

module Subst_context (Instance : sig
  val instance_desc : term_desc -> term_desc Instance_context.t
end) : sig
  type 'a subst_context
  type 'a t = 'a subst_context

  (* monad *)
  val test :
    loc:Warnings.loc ->
    offset:Offset.t ->
    from:Offset.t ->
    to_:term_desc ->
    (unit -> 'a subst_context) ->
    ('a, error) result

  val return : 'a -> 'a subst_context
  val bind : 'a subst_context -> ('a -> 'b subst_context) -> 'b subst_context

  val ( let* ) :
    'a subst_context -> ('a -> 'b subst_context) -> 'b subst_context

  val ( let+ ) : 'a subst_context -> ('a -> 'b) -> 'b subst_context

  (* from *)
  val from : unit -> Offset.t subst_context
  val with_binder : (unit -> 'a subst_context) -> 'a subst_context

  (* to_ *)
  val to_ : unit -> term_desc subst_context
end

module Normalize_context (Instance : sig
  val instance_desc : term_desc -> term_desc Instance_context.t
end) (Subst : sig
  val subst_term : term -> term Subst_context(Instance).t
  val subst_type : type_ -> type_ Subst_context(Instance).t
  val subst_desc : term_desc -> term_desc Subst_context(Instance).t
  val subst_annot : annot -> annot Subst_context(Instance).t
  val subst_bind : bind -> bind Subst_context(Instance).t
end) : sig
  type 'a normalize_context
  type 'a t = 'a normalize_context

  (* monad *)
  val test :
    loc:Warnings.loc -> (unit -> 'a normalize_context) -> ('a, error) result

  val return : 'a -> 'a normalize_context

  val bind :
    'a normalize_context -> ('a -> 'b normalize_context) -> 'b normalize_context

  val ( let* ) :
    'a normalize_context -> ('a -> 'b normalize_context) -> 'b normalize_context

  val ( let+ ) : 'a normalize_context -> ('a -> 'b) -> 'b normalize_context

  (* subst *)
  val subst_term :
    from:Offset.t -> to_:term_desc -> term -> term normalize_context

  val subst_type :
    from:Offset.t -> to_:term_desc -> type_ -> type_ normalize_context

  val subst_desc :
    from:Offset.t -> to_:term_desc -> term_desc -> term_desc normalize_context

  val subst_annot :
    from:Offset.t -> to_:term_desc -> annot -> annot normalize_context

  val subst_bind :
    from:Offset.t -> to_:term_desc -> bind -> bind normalize_context
end

(* TODO: this is bad *)
module Unify_context (Instance : sig
  val instance_desc : term_desc -> term_desc Instance_context.t
end) (Subst : sig
  val subst_term : term -> term Subst_context(Instance).t
  val subst_type : type_ -> type_ Subst_context(Instance).t
  val subst_desc : term_desc -> term_desc Subst_context(Instance).t
  val subst_annot : annot -> annot Subst_context(Instance).t
  val subst_bind : bind -> bind Subst_context(Instance).t
end) (Normalize : sig
  val normalize_term : term -> term Normalize_context(Instance)(Subst).t
  val normalize_type : type_ -> type_ Normalize_context(Instance)(Subst).t
end) : sig
  type 'a unify_context
  type 'a t = 'a unify_context

  (* monad *)
  val test :
    loc:Warnings.loc -> (unit -> 'a unify_context) -> ('a, error) result

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
end

module Typer_context (Instance : sig
  val instance_term : term -> term Instance_context.t
  val instance_type : type_ -> type_ Instance_context.t
  val instance_desc : term_desc -> term_desc Instance_context.t
end) (Subst : sig
  val subst_term : term -> term Subst_context(Instance).t
  val subst_type : type_ -> type_ Subst_context(Instance).t
  val subst_desc : term_desc -> term_desc Subst_context(Instance).t
  val subst_annot : annot -> annot Subst_context(Instance).t
  val subst_bind : bind -> bind Subst_context(Instance).t
end) (Normalize : sig
  val normalize_term : term -> term Normalize_context(Instance)(Subst).t
  val normalize_type : type_ -> type_ Normalize_context(Instance)(Subst).t
end) (Unify : sig
  val unify_type :
    expected:type_ ->
    received:type_ ->
    unit Unify_context(Instance)(Subst)(Normalize).t
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

  (* subst *)
  val subst_type :
    from:Offset.t -> to_:term_desc -> type_ -> type_ typer_context

  (* lower *)
  val lower_term : offset:Offset.t -> term -> term typer_context
  val lower_type : offset:Offset.t -> type_ -> type_ typer_context

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
