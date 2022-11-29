open Ttree

type error = CError of { loc : Location.t; desc : error_desc }

and error_desc =
  (* typer *)
  | CError_typer_pat_not_annotated of { pat : Ltree.pat_desc }
  | CError_typer_pat_not_pair of { pat : Ltree.pat_desc; expected : type_ }
  (* unify *)
  | CError_unify_var_clash of { expected : Offset.t; received : Offset.t }
  | CError_unify_type_clash of { expected : term_desc; received : term_desc }
  | CError_unify_pat_clash of { expected : pat_desc; received : pat_desc }
  (* typer *)
  | CError_typer_unknown_var of { var : Name.t }
  | Cerror_typer_not_a_forall of { type_ : type_ }
  | Cerror_typer_not_an_exists of { type_ : type_ }

module Normalize_context = struct
  type var_info = Subst of { to_ : term_desc } | Bound of { base : Offset.t }

  type 'a normalize_context = {
    (* TODO: accumulate locations during normalization *)
    context :
      'k.
      vars:var_info list ->
      offset:Offset.t ->
      ok:('a -> 'k) ->
      error:(error_desc -> 'k) ->
      'k;
  }
  [@@ocaml.unboxed]

  type 'a t = 'a normalize_context

  let[@inline always] test ~loc ~vars ~offset f =
    let { context } = f () in
    let ok value = Ok value in
    let error desc = Error (CError { loc; desc }) in
    context ~vars ~offset ~ok ~error

  let[@inline always] return value =
    let context ~vars:_ ~offset:_ ~ok ~error:_ = ok value in
    { context }

  let[@inline always] bind context f =
    let { context } = context in
    let context ~vars ~offset ~ok ~error =
      let ok data =
        let { context } = f data in
        context ~vars ~offset ~ok ~error
      in
      context ~vars ~offset ~ok ~error
    in
    { context }

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] repr_var ~var =
    let context ~vars ~offset ~ok ~error:_ =
      match
        (* TODO: should this be var + offset? *)
        let index = Offset.(repr (var - one)) in
        List.nth_opt vars index
      with
      | Some (Subst { to_ }) ->
          let offset = Offset.(var + offset) in
          ok (TT_offset { desc = to_; offset })
      | Some (Bound { base }) ->
          let offset = Offset.(var + offset - base) in
          ok (TT_var { offset })
      | None | (exception Invalid_argument _) ->
          (* free var *)
          let offset = Offset.(var + offset) in
          ok (TT_var { offset })
    in
    { context }

  let[@inline always] with_var f =
    let context ~vars ~offset ~ok ~error =
      let vars = Bound { base = offset } :: vars in
      let { context } = f () in
      context ~vars ~offset ~ok ~error
    in
    { context }

  let[@inline always] elim_var ~to_ f =
    let context ~vars ~offset ~ok ~error =
      let vars = Subst { to_ } :: vars in
      let { context } = f () in
      context ~vars ~offset ~ok ~error
    in
    { context }

  let[@inline always] with_offset ~offset f =
    let context ~vars ~offset:current_offset ~ok ~error =
      let offset = Offset.(current_offset + offset) in
      let { context } = f () in
      context ~vars ~offset ~ok ~error
    in
    { context }
end

module Unify_context (Normalize : sig
  val normalize_term : term -> term Normalize_context.t
  val normalize_type : type_ -> type_ Normalize_context.t
end) =
struct
  type 'a unify_context = {
    (* TODO: accumulate locations during unification *)
    context :
      'k.
      expected_offset:Offset.t ->
      received_offset:Offset.t ->
      ok:('a -> 'k) ->
      error:(error_desc -> 'k) ->
      'k;
  }
  [@@ocaml.unboxed]

  type 'a t = 'a unify_context

  let[@inline always] test ~loc ~expected_offset ~received_offset f =
    let { context } = f () in
    let ok value = Ok value in
    let error desc = Error (CError { loc; desc }) in
    context ~expected_offset ~received_offset ~ok ~error

  let[@inline always] return value =
    let context ~expected_offset:_ ~received_offset:_ ~ok ~error:_ = ok value in
    { context }

  let[@inline always] bind context f =
    let { context } = context in
    let context ~expected_offset ~received_offset ~ok ~error =
      let ok data =
        let { context } = f data in
        context ~expected_offset ~received_offset ~ok ~error
      in
      context ~expected_offset ~received_offset ~ok ~error
    in
    { context }

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] error_var_clash ~expected ~received =
    let context ~expected_offset:_ ~received_offset:_ ~ok:_ ~error =
      error (CError_unify_var_clash { expected; received })
    in
    { context }

  let[@inline always] error_type_clash ~expected ~received =
    let context ~expected_offset:_ ~received_offset:_ ~ok:_ ~error =
      error (CError_unify_type_clash { expected; received })
    in
    { context }

  let[@inline always] error_pat_clash ~expected ~received =
    let context ~expected_offset:_ ~received_offset:_ ~ok:_ ~error =
      error (CError_unify_pat_clash { expected; received })
    in
    { context }

  let[@inline always] with_normalize_context f =
    let context ~expected_offset:_ ~received_offset:_ ~ok ~error =
      let Normalize_context.{ context } = f () in
      let offset = Offset.zero in
      context ~vars:[] ~offset ~ok ~error
    in
    { context }

  let[@inline always] normalize_term term =
    with_normalize_context @@ fun () -> Normalize.normalize_term term

  let[@inline always] normalize_type type_ =
    with_normalize_context @@ fun () -> Normalize.normalize_type type_

  let[@inline always] repr_expected_var ~var =
    let context ~expected_offset ~received_offset:_ ~ok ~error:_ =
      ok Offset.(expected_offset + var)
    in
    { context }

  let[@inline always] repr_received_var ~var =
    let context ~expected_offset:_ ~received_offset ~ok ~error:_ =
      ok Offset.(received_offset + var)
    in
    { context }

  let[@inline always] with_expected_offset ~offset f =
    let context ~expected_offset ~received_offset ~ok ~error =
      let expected_offset = Offset.(expected_offset + offset) in
      let { context } = f () in
      context ~expected_offset ~received_offset ~ok ~error
    in
    { context }

  let[@inline always] with_received_offset ~offset f =
    let context ~expected_offset ~received_offset ~ok ~error =
      let received_offset = Offset.(received_offset + offset) in
      let { context } = f () in
      context ~expected_offset ~received_offset ~ok ~error
    in
    { context }
end

module Typer_context (Normalize : sig
  val normalize_term : term -> term Normalize_context.t
  val normalize_type : type_ -> type_ Normalize_context.t
end) (Unify : sig
  val unify_type :
    expected:type_ -> received:type_ -> unit Unify_context(Normalize).t
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

  let[@inline always] run ~loc f =
    let { context } = f () in
    let type_of_types = Level.zero in
    let level = Level.next type_of_types in
    (* TODO: meaningful size?
        - parser counts binders
        - estimate based on the file size *)
    let names = Name.Tbl.create 1024 in
    (let type_name = Name.make "Type" in
     let type_ =
       (* TODO: breaking abstraction *)
       let desc = TT_var { offset = Offset.zero } in
       TType { loc; desc }
     in
     (* TODO: better place for constants *)
     Name.Tbl.add names type_name (type_of_types, type_));
    let ok value = Ok value in
    let error desc = Error (CError { loc; desc }) in
    context ~loc ~type_of_types ~level ~names ~ok ~error

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

  let[@inline always] error_pat_not_annotated ~pat =
    let context ~loc:_ ~type_of_types:_ ~level:_ ~names:_ ~ok:_ ~error =
      error (CError_typer_pat_not_annotated { pat })
    in
    { context }

  let[@inline always] error_typer_pat_not_pair ~pat ~expected =
    let context ~loc:_ ~type_of_types:_ ~level:_ ~names:_ ~ok:_ ~error =
      error (CError_typer_pat_not_pair { pat; expected })
    in
    { context }

  let[@inline always] instance ~var =
    let context ~loc:_ ~type_of_types:_ ~level ~names ~ok ~error =
      match Name.Tbl.find_opt names var with
      | Some (var_level, type_) ->
          let offset = Level.offset ~from:var_level ~to_:level in
          (* TODO: breaking abstraction *)
          let (TType { loc; desc }) = type_ in
          let desc = TT_offset { desc; offset } in
          let type_ = TType { loc; desc } in
          ok (offset, type_)
      | None -> error (CError_typer_unknown_var { var })
    in
    { context }

  let[@inline always] with_binder ~var ~type_ f =
    let context ~loc ~type_of_types ~level ~names ~ok ~error =
      Name.Tbl.add names var (level, type_);
      let level = Level.next level in
      let { context } = f () in
      let ok value =
        Name.Tbl.remove names var;
        ok value
      in
      context ~loc ~type_of_types ~level ~names ~ok ~error
    in
    { context }

  module Unify_context = Unify_context (Normalize)

  let unify_type ~expected ~received =
    let context ~loc:_ ~type_of_types:_ ~level:_ ~names:_ ~ok ~error =
      let Unify_context.{ context } = Unify.unify_type ~expected ~received in
      context ~expected_offset:Offset.zero ~received_offset:Offset.zero ~ok
        ~error
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
    let context ~loc ~type_of_types ~level ~names:_ ~ok ~error =
      let tt_type = tt_type ~loc ~type_of_types ~level in
      let (TTerm { loc; desc; type_ }) = term in
      let Unify_context.{ context } =
        Unify.unify_type ~expected:tt_type ~received:type_
      in
      let ok () =
        (* TODO: breaking abstraction *)
        ok (TType { loc; desc })
      in
      context ~expected_offset:Offset.zero ~received_offset:Offset.zero ~ok
        ~error
    in
    { context }

  let[@inline always] with_normalize_context f =
    let context ~loc:_ ~type_of_types:_ ~level:_ ~names:_ ~ok ~error =
      let Normalize_context.{ context } = f () in
      context ~vars:[] ~offset:Offset.zero ~ok ~error
    in
    { context }

  let[@inline always] normalize_type type_ =
    with_normalize_context @@ fun () -> Normalize.normalize_type type_

  let[@inline always] split_forall type_ =
    let* type_ = normalize_type type_ in
    (* TODO: two normalize guarantees no TT_offset? *)
    let* type_ = normalize_type type_ in
    let context ~loc:_ ~type_of_types:_ ~level:_ ~names:_ ~ok ~error =
      (* TODO: what about this location? *)
      let (TType { loc = _; desc }) = type_ in
      match desc with
      | TT_forall { param; return } -> ok (param, return)
      | TT_var _ | TT_lambda _ | TT_apply _ | TT_exists _ | TT_pair _ | TT_let _
      | TT_annot _ | TT_offset _ ->
          error (Cerror_typer_not_a_forall { type_ })
    in
    { context }

  let[@inline always] split_exists type_ =
    let* type_ = normalize_type type_ in
    (* TODO: two normalize guarantees no TT_offset? *)
    let* type_ = normalize_type type_ in
    let context ~loc:_ ~type_of_types:_ ~level:_ ~names:_ ~ok ~error =
      (* TODO: what about this location? *)
      let (TType { loc = _; desc }) = type_ in
      match desc with
      | TT_exists { left; right } -> ok (left, right)
      | TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_pair _ | TT_let _
      | TT_annot _ | TT_offset _ ->
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

  let[@inline always] tp_var type_ ~var =
    let context ~loc ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tp_var loc type_ ~var
    in
    { context }

  let[@inline always] tp_pair type_ ~left ~right =
    let context ~loc ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tp_pair loc type_ ~left ~right
    in
    { context }

  let[@inline always] tp_annot ~pat ~annot =
    let context ~loc ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tp_annot loc ~pat ~annot
    in
    { context }

  let[@inline always] tannot ~pat ~annot =
    let context ~loc ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tannot loc ~pat ~annot
    in
    { context }

  let[@inline always] tbind ~pat ~value =
    let context ~loc ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tbind loc ~pat ~value
    in
    { context }
end
