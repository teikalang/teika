open Ttree

type error = CError of { loc : Location.t; desc : error_desc }

and error_desc =
  (* unify *)
  | CError_unify_var_clash of { expected : Offset.t; received : Offset.t }
  | CError_unify_type_clash of { expected : term_desc; received : term_desc }

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

module Normalize_context (Subst : sig
  val subst_term : term -> term Subst_context.t
  val subst_type : type_ -> type_ Subst_context.t
  val subst_desc : term_desc -> term_desc Subst_context.t
  val subst_annot : annot -> annot Subst_context.t
  val subst_bind : bind -> bind Subst_context.t
end) =
struct
  type 'a normalize_context = {
    (* TODO: accumulate locations during normalization *)
    context : 'k. ok:('a -> 'k) -> error:(error_desc -> 'k) -> 'k;
  }
  [@@ocaml.unboxed]

  type 'a t = 'a normalize_context

  let[@inline always] test ~loc f =
    let { context } = f () in
    let ok value = Ok value in
    let error desc = Error (CError { loc; desc }) in
    context ~ok ~error

  let[@inline always] return value =
    let context ~ok ~error:_ = ok value in
    { context }

  let[@inline always] bind context f =
    let { context } = context in
    let context ~ok ~error =
      let ok data =
        let { context } = f data in
        context ~ok ~error
      in
      context ~ok ~error
    in
    { context }

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] with_subst_context ~from ~to_ f =
    let context ~ok ~error =
      let Subst_context.{ context } = f () in
      context ~from ~to_ ~ok ~error
    in
    { context }

  let[@inline always] subst_term ~from ~to_ term =
    with_subst_context ~from ~to_ @@ fun () -> Subst.subst_term term

  let[@inline always] subst_type ~from ~to_ type_ =
    with_subst_context ~from ~to_ @@ fun () -> Subst.subst_type type_

  let[@inline always] subst_desc ~from ~to_ desc =
    with_subst_context ~from ~to_ @@ fun () -> Subst.subst_desc desc

  let[@inline always] subst_annot ~from ~to_ annot =
    with_subst_context ~from ~to_ @@ fun () -> Subst.subst_annot annot

  let[@inline always] subst_bind ~from ~to_ bind =
    with_subst_context ~from ~to_ @@ fun () -> Subst.subst_bind bind
end

module Unify_context (Subst : sig
  val subst_term : term -> term Subst_context.t
  val subst_type : type_ -> type_ Subst_context.t
  val subst_desc : term_desc -> term_desc Subst_context.t
  val subst_annot : annot -> annot Subst_context.t
  val subst_bind : bind -> bind Subst_context.t
end) (Normalize : sig
  val normalize_term : term -> term Normalize_context(Subst).t
  val normalize_type : type_ -> type_ Normalize_context(Subst).t
end) =
struct
  module Normalize_context = Normalize_context (Subst)

  type 'a unify_context = {
    (* TODO: accumulate locations during unification *)
    context : 'k. ok:('a -> 'k) -> error:(error_desc -> 'k) -> 'k;
  }
  [@@ocaml.unboxed]

  type 'a t = 'a unify_context

  let[@inline always] test ~loc f =
    let { context } = f () in
    let ok value = Ok value in
    let error desc = Error (CError { loc; desc }) in
    context ~ok ~error

  let[@inline always] return value =
    let context ~ok ~error:_ = ok value in
    { context }

  let[@inline always] bind context f =
    let { context } = context in
    let context ~ok ~error =
      let ok data =
        let { context } = f data in
        context ~ok ~error
      in
      context ~ok ~error
    in
    { context }

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] error_var_clash ~expected ~received =
    let context ~ok:_ ~error =
      error (CError_unify_var_clash { expected; received })
    in
    { context }

  let[@inline always] error_type_clash ~expected ~received =
    let context ~ok:_ ~error =
      error (CError_unify_type_clash { expected; received })
    in
    { context }

  let[@inline always] with_normalize_context f =
    let context ~ok ~error =
      let Normalize_context.{ context } = f () in
      context ~ok ~error
    in
    { context }

  let[@inline always] normalize_term term =
    with_normalize_context @@ fun () -> Normalize.normalize_term term

  let[@inline always] normalize_type type_ =
    with_normalize_context @@ fun () -> Normalize.normalize_type type_
end

module Instance_context = struct
  type 'a instance_context = {
    (* TODO: accumulate locations during instantiation *)
    context :
      'k. offset:Offset.t -> ok:('a -> 'k) -> error:(error_desc -> 'k) -> 'k;
  }
  [@@ocaml.unboxed]

  type 'a t = 'a instance_context

  let[@inline always] test ~loc ~offset f =
    let { context } = f () in
    let ok value = Ok value in
    let error desc = Error (CError { loc; desc }) in
    context ~offset ~ok ~error

  let[@inline always] return value =
    let context ~offset:_ ~ok ~error:_ = ok value in
    { context }

  let[@inline always] bind context f =
    let { context } = context in
    let context ~offset ~ok ~error =
      let ok data =
        let { context } = f data in
        context ~offset ~ok ~error
      in
      context ~offset ~ok ~error
    in
    { context }

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] offset () =
    let context ~offset ~ok ~error:_ = ok offset in
    { context }
end
