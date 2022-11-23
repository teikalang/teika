open Ttree

type error = private CError of { loc : Location.t; desc : error_desc }
and error_desc = private |

module Subst_context : sig
  type 'a subst_context
  type 'a t = 'a subst_context

  (* monad *)
  val test :
    loc:Warnings.loc ->
    from:Offset.offset ->
    to_:term_desc ->
    (unit -> 'a t) ->
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
  val test : loc:Warnings.loc -> (unit -> 'a t) -> ('a, error) result
  val return : 'a -> 'a normalize_context

  val bind :
    'a normalize_context -> ('a -> 'b normalize_context) -> 'b normalize_context

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

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
