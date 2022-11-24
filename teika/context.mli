open Ttree

type error = private CError of { loc : Location.t; desc : error_desc }

and error_desc = private
  (* unify *)
  | CError_unify_var_clash of { expected : Offset.t; received : Offset.t }
  | CError_unify_type_clash of { expected : term_desc; received : term_desc }

module Subst_context : sig
  type 'a subst_context
  type 'a t = 'a subst_context

  (* monad *)
  val test :
    loc:Warnings.loc ->
    from:Offset.offset ->
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

module Normalize_context (Subst : sig
  val subst_term : term -> term Subst_context.t
  val subst_type : type_ -> type_ Subst_context.t
  val subst_desc : term_desc -> term_desc Subst_context.t
  val subst_annot : annot -> annot Subst_context.t
  val subst_bind : bind -> bind Subst_context.t
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
module Unify_context (Subst : sig
  val subst_term : term -> term Subst_context.t
  val subst_type : type_ -> type_ Subst_context.t
  val subst_desc : term_desc -> term_desc Subst_context.t
  val subst_annot : annot -> annot Subst_context.t
  val subst_bind : bind -> bind Subst_context.t
end) (Normalize : sig
  val normalize_term : term -> term Normalize_context(Subst).t
  val normalize_type : type_ -> type_ Normalize_context(Subst).t
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
