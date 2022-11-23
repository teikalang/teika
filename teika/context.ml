open Ttree

type error = CError of { loc : Location.t; desc : error_desc }
and error_desc = |

module Subst_context = struct
  type 'a subst_context = {
    (* TODO: accumulate locations during substitutions *)
    context :
      'k.
      from:Offset.t ->
      to_:term_desc ->
      ok:('a -> 'k) ->
      error:(error_desc -> 'k) ->
      'k;
  }
  [@@ocaml.unboxed]

  type 'a t = 'a subst_context

  let[@inline always] test ~loc ~from ~to_ f =
    let { context } = f () in
    let ok value = Ok value in
    let error desc = Error (CError { loc; desc }) in
    context ~from ~to_ ~ok ~error

  let[@inline always] return value =
    let context ~from:_ ~to_:_ ~ok ~error:_ = ok value in
    { context }

  let[@inline always] bind context f =
    let { context } = context in
    let context ~from ~to_ ~ok ~error =
      let ok data =
        let { context } = f data in
        context ~from ~to_ ~ok ~error
      in
      context ~from ~to_ ~ok ~error
    in
    { context }

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] from () =
    let context ~from ~to_:_ ~ok ~error:_ = ok from in
    { context }

  let[@inline always] to_ () =
    let context ~from:_ ~to_ ~ok ~error:_ = ok to_ in
    { context }

  let[@inline always] with_binder f =
    let context ~from ~to_ ~ok ~error =
      let from = Offset.(from + one) in
      let { context } = f () in
      context ~from ~to_ ~ok ~error
    in
    { context }
end
