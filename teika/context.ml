open Ttree

type error = CError of { loc : Location.t; desc : error_desc }

and error_desc =
  (* unify *)
  | CError_unify_var_clash of { expected : Offset.t; received : Offset.t }
  | CError_unify_type_clash of { expected : term_desc; received : term_desc }
  (* typer *)
  | CError_typer_unknown_var of { var : Name.t }
  | CError_typer_term_not_a_type of { term : term }
  | Cerror_typer_not_a_forall of { type_ : type_ }
  | Cerror_typer_not_an_exists of { type_ : type_ }

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

module Subst_context (Instance : sig
  val instance_desc : term_desc -> term_desc Instance_context.t
end) =
struct
  type 'a subst_context = {
    (* TODO: accumulate locations during substitutions *)
    context :
      'k.
      offset:Offset.t ->
      from:Offset.t ->
      to_:term_desc ->
      ok:('a -> 'k) ->
      error:(error_desc -> 'k) ->
      'k;
  }
  [@@ocaml.unboxed]

  type 'a t = 'a subst_context

  let[@inline always] test ~loc ~offset ~from ~to_ f =
    let { context } = f () in
    let ok value = Ok value in
    let error desc = Error (CError { loc; desc }) in
    context ~offset ~from ~to_ ~ok ~error

  let[@inline always] return value =
    let context ~offset:_ ~from:_ ~to_:_ ~ok ~error:_ = ok value in
    { context }

  let[@inline always] bind context f =
    let { context } = context in
    let context ~offset ~from ~to_ ~ok ~error =
      let ok data =
        let { context } = f data in
        context ~offset ~from ~to_ ~ok ~error
      in
      context ~offset ~from ~to_ ~ok ~error
    in
    { context }

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] from () =
    let context ~offset ~from ~to_:_ ~ok ~error:_ = ok Offset.(from + offset) in
    { context }

  let[@inline always] to_ () =
    let context ~offset ~from:_ ~to_ ~ok ~error =
      let Instance_context.{ context } = Instance.instance_desc to_ in
      context ~offset ~ok ~error
    in
    { context }

  let[@inline always] with_binder f =
    let context ~offset ~from ~to_ ~ok ~error =
      let offset = Offset.(offset + one) in
      let { context } = f () in
      context ~offset ~from ~to_ ~ok ~error
    in
    { context }
end

module Normalize_context (Instance : sig
  val instance_desc : term_desc -> term_desc Instance_context.t
end) (Subst : sig
  val subst_term : term -> term Subst_context(Instance).t
  val subst_type : type_ -> type_ Subst_context(Instance).t
  val subst_desc : term_desc -> term_desc Subst_context(Instance).t
  val subst_annot : annot -> annot Subst_context(Instance).t
  val subst_bind : bind -> bind Subst_context(Instance).t
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

  module Subst_context = Subst_context (Instance)

  let[@inline always] with_subst_context ~from ~to_ f =
    let context ~ok ~error =
      let Subst_context.{ context } = f () in
      (* TODO: is this always right? *)
      let offset = Offset.zero in
      context ~offset ~from ~to_ ~ok ~error
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
end) =
struct
  module Normalize_context = Normalize_context (Instance) (Subst)

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

module Typer_context (Instance : sig
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
end) =
struct
  type 'a typer_context = {
    (* TODO: accumulate locations during instantiation *)
    context :
      'k.
      loc:Location.t ->
      type_of_types:Level.t ->
      level:Level.t ->
      names:(Level.t * type_) Name.Tbl.t ->
      ok:('a -> 'k) ->
      error:(error_desc -> 'k) ->
      'k;
  }
  [@@ocaml.unboxed]

  type 'a t = 'a typer_context

  let[@inline always] test ~loc ~type_of_types ~level ~names f =
    let { context } = f () in
    let ok value = Ok value in
    let error desc = Error (CError { loc; desc }) in
    context ~loc ~type_of_types ~level ~names ~ok ~error

  let[@inline always] return value =
    let context ~loc:_ ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok value
    in
    { context }

  let[@inline always] bind context f =
    let { context } = context in
    let context ~loc ~type_of_types ~level ~names ~ok ~error =
      let ok data =
        let { context } = f data in
        context ~loc ~type_of_types ~level ~names ~ok ~error
      in
      context ~loc ~type_of_types ~level ~names ~ok ~error
    in
    { context }

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let instance ~var =
    let context ~loc:_ ~type_of_types:_ ~level ~names ~ok ~error =
      match Name.Tbl.find_opt names var with
      | Some (var_level, type_) ->
          let offset = Level.offset ~from:var_level ~to_:level in
          let Instance_context.{ context } = Instance.instance_type type_ in
          let ok type_ = ok (offset, type_) in
          context ~offset ~ok ~error
      | None -> error (CError_typer_unknown_var { var })
    in
    { context }

  let[@inline always] with_binder ~var ~type_ f =
    let context ~loc ~type_of_types ~level ~names ~ok ~error =
      let level = Level.next level in
      Name.Tbl.add names var (level, type_);
      let { context } = f () in
      let ok value =
        Name.Tbl.remove names var;
        ok value
      in
      context ~loc ~type_of_types ~level ~names ~ok ~error
    in
    { context }

  module Subst_context = Subst_context (Instance)

  let[@inline always] with_subst_context ~from ~to_ f =
    let context ~loc:_ ~type_of_types:_ ~level:_ ~names:_ ~ok ~error =
      let offset = Offset.zero in
      let Subst_context.{ context } = f () in
      context ~offset ~from ~to_ ~ok ~error
    in
    { context }

  let[@inline always] subst_type ~from ~to_ type_ =
    with_subst_context ~from ~to_ @@ fun () -> Subst.subst_type type_

  module Unify_context = Unify_context (Instance) (Subst) (Normalize)

  let unify_type ~expected ~received =
    let context ~loc:_ ~type_of_types:_ ~level:_ ~names:_ ~ok ~error =
      let Unify_context.{ context } = Unify.unify_type ~expected ~received in
      context ~ok ~error
    in
    { context }

  let[@inline always] with_loc ~loc f =
    let context ~loc:_parent_loc ~type_of_types ~level ~names ~ok ~error =
      let { context } = f () in
      context ~loc ~type_of_types ~level ~names ~ok ~error
    in
    { context }

  open Ttree

  let tt_type_offset ~type_of_types ~level =
    Level.offset ~from:type_of_types ~to_:level

  let tt_type ~loc ~type_of_types ~level =
    let offset = tt_type_offset ~type_of_types ~level in
    (* TODO: breaking abstraction *)
    let desc = TT_var { offset } in
    TType { loc; desc }

  let[@inline always] term_of_type type_ =
    let context ~loc:_ ~type_of_types ~level ~names:_ ~ok ~error:_ =
      let (TType { loc; desc }) = type_ in
      let type_ = tt_type ~loc ~type_of_types ~level in
      (* TODO: breaking abstraction *)
      ok (TTerm { loc; desc; type_ })
    in
    { context }

  let[@inline always] type_of_term term =
    let context ~loc:_ ~type_of_types ~level ~names:_ ~ok ~error =
      let (TTerm { loc; desc; type_ }) = term in
      (* TODO: this should probably use unify *)
      let type_offset = tt_type_offset ~type_of_types ~level in
      match
        let (TType { loc = _; desc }) = type_ in
        desc
      with
      | TT_var { offset } -> (
          match Offset.equal offset type_offset with
          | true ->
              (* TODO: breaking abstraction *)
              ok (TType { loc; desc })
          | false -> error (CError_typer_term_not_a_type { term }))
      | TT_forall _ | TT_lambda _ | TT_apply _ | TT_exists _ | TT_pair _
      | TT_unpair _ | TT_let _ | TT_annot _ ->
          error (CError_typer_term_not_a_type { term })
    in
    { context }

  let[@inline always] split_forall type_ =
    let context ~loc:_ ~type_of_types:_ ~level:_ ~names:_ ~ok ~error =
      (* TODO: what about this location? *)
      let (TType { loc = _; desc }) = type_ in
      match desc with
      | TT_forall { param; return } -> ok (param, return)
      | TT_var _ | TT_lambda _ | TT_apply _ | TT_exists _ | TT_pair _
      | TT_unpair _ | TT_let _ | TT_annot _ ->
          error (Cerror_typer_not_a_forall { type_ })
    in
    { context }

  let[@inline always] split_exists type_ =
    let context ~loc:_ ~type_of_types:_ ~level:_ ~names:_ ~ok ~error =
      (* TODO: what about this location? *)
      let (TType { loc = _; desc }) = type_ in
      match desc with
      | TT_exists { left; right } -> ok (left, right)
      | TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_pair _
      | TT_unpair _ | TT_let _ | TT_annot _ ->
          error (Cerror_typer_not_an_exists { type_ })
    in
    { context }

  let[@inline always] tt_type () =
    let context ~loc ~type_of_types ~level ~names:_ ~ok ~error:_ =
      ok (tt_type ~loc ~type_of_types ~level)
    in
    { context }

  let[@inline always] tt_var type_ ~offset =
    let context ~loc ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tt_var loc type_ ~offset
    in
    { context }

  let[@inline always] tt_forall ~param ~return =
    let context ~loc ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tt_forall loc ~param ~return
    in
    { context }

  let[@inline always] tt_lambda type_ ~param ~return =
    let context ~loc ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tt_lambda loc type_ ~param ~return
    in
    { context }

  let[@inline always] tt_apply type_ ~lambda ~arg =
    let context ~loc ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tt_apply loc type_ ~lambda ~arg
    in
    { context }

  let[@inline always] tt_exists ~left ~right =
    let context ~loc ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tt_exists loc ~left ~right
    in
    { context }

  let[@inline always] tt_pair type_ ~left ~right =
    let context ~loc ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tt_pair loc type_ ~left ~right
    in
    { context }

  let[@inline always] tt_unpair type_ ~left ~right ~pair ~return =
    let context ~loc ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tt_unpair loc type_ ~left ~right ~pair ~return
    in
    { context }

  let[@inline always] tt_let type_ ~bound ~return =
    let context ~loc ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tt_let loc type_ ~bound ~return
    in
    { context }

  let[@inline always] tt_annot ~value ~annot =
    let context ~loc ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tt_annot loc ~value ~annot
    in
    { context }

  let[@inline always] tannot ~var ~annot =
    let context ~loc ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tannot loc ~var ~annot
    in
    { context }

  let[@inline always] tbind ~var ~value =
    let context ~loc ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tbind loc ~var ~value
    in
    { context }
end
