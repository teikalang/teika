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
