open Ttree

type error =
  (* TODO: why track nested locations?
      Probably because things like macros exists *)
  | CError_loc of { error : error; loc : Location.t [@opaque] }
  (* unify *)
  | CError_unify_var_clash of { expected : Offset.t; received : Offset.t }
  | CError_unify_type_clash of {
      expected : term; [@printer Tprinter.pp_term]
      received : term; [@printer Tprinter.pp_term]
    }
  | CError_unify_pat_clash of {
      expected : pat; [@printer Tprinter.pp_pat]
      received : pat; [@printer Tprinter.pp_pat]
    }
  | CError_unify_var_escape_scope of { var : Offset.t }
  (* typer *)
  | CError_typer_unknown_var of { var : Name.t }
  | Cerror_typer_not_a_forall of { type_ : term [@printer Tprinter.pp_term] }
  | CError_typer_pat_not_annotated of { pat : Ltree.pat }
  | CError_typer_pairs_not_implemented
  (* invariant *)
  | CError_typer_term_var_not_annotated of { var : Offset.t }
  | CError_typer_pat_var_not_annotated of { var : Name.t }
[@@deriving show { with_path = false }]

module Normalize_context = struct
  type var_info = Subst of { to_ : term } | Bound of { base : Offset.t }

  type 'a normalize_context = {
    (* TODO: accumulate locations during normalization *)
    context :
      'k.
      vars:var_info list ->
      offset:Offset.t ->
      ok:('a -> 'k) ->
      error:(error -> 'k) ->
      'k;
  }
  [@@ocaml.unboxed]

  type 'a t = 'a normalize_context

  let[@inline always] test ~vars ~offset f =
    let { context } = f () in
    let ok value = Ok value in
    let error error = Error error in
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
          ok (TT_offset { term = to_; offset })
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
end) =
struct
  type 'a unify_context = {
    (* TODO: accumulate locations during unification *)
    context :
      'k.
      expected_offset:Offset.t ->
      received_offset:Offset.t ->
      ok:('a -> 'k) ->
      error:(error -> 'k) ->
      'k;
  }
  [@@ocaml.unboxed]

  type 'a t = 'a unify_context

  let[@inline always] test ~expected_offset ~received_offset f =
    let { context } = f () in
    let ok value = Ok value in
    let error error = Error error in
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

  let[@inline always] error_var_escape_scope ~var =
    let context ~expected_offset:_ ~received_offset:_ ~ok:_ ~error =
      error (CError_unify_var_escape_scope { var })
    in
    { context }

  let[@inline always] with_normalize_context f =
    let context ~expected_offset:_ ~received_offset:_ ~ok ~error =
      let Normalize_context.{ context } = f () in
      let offset = Offset.zero in
      let ok value = ok value in
      let error desc = error desc in
      context ~vars:[] ~offset ~ok ~error
    in
    { context }

  let[@inline always] normalize_term term =
    with_normalize_context @@ fun () -> Normalize.normalize_term term

  let[@inline always] expected_offset () =
    let context ~expected_offset ~received_offset:_ ~ok ~error:_ =
      ok expected_offset
    in
    { context }

  let[@inline always] received_offset () =
    let context ~expected_offset:_ ~received_offset ~ok ~error:_ =
      ok received_offset
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
end) (Unify : sig
  val unify_term :
    expected:term -> received:term -> unit Unify_context(Normalize).t
end) =
struct
  type 'a typer_context = {
    (* TODO: accumulate locations during instantiation *)
    context :
      'k.
      type_of_types:Level.t ->
      level:Level.t ->
      names:(Level.t * term) Name.Tbl.t ->
      ok:('a -> 'k) ->
      error:(error -> 'k) ->
      'k;
  }
  [@@ocaml.unboxed]

  type 'a t = 'a typer_context

  let[@inline always] run f =
    let { context } = f () in
    let type_of_types = Level.zero in
    let level = Level.next type_of_types in
    (* TODO: meaningful size?
        - parser counts binders
        - estimate based on the file size *)
    let names = Name.Tbl.create 1024 in
    (let type_name = Name.make "Type" in
     let type_ = TT_var { offset = Offset.zero } in
     let type_ = TT_annot { term = type_; annot = type_ } in
     (* TODO: better place for constants *)
     Name.Tbl.add names type_name (type_of_types, type_));
    let ok value = Ok value in
    let error error = Error error in
    context ~type_of_types ~level ~names ~ok ~error

  let[@inline always] test ~type_of_types ~level ~names f =
    let { context } = f () in
    let ok value = Ok value in
    let error error = Error error in
    context ~type_of_types ~level ~names ~ok ~error

  let[@inline always] return value =
    let context ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ = ok value in
    { context }

  let[@inline always] bind context f =
    let { context } = context in
    let context ~type_of_types ~level ~names ~ok ~error =
      let ok data =
        let { context } = f data in
        context ~type_of_types ~level ~names ~ok ~error
      in
      context ~type_of_types ~level ~names ~ok ~error
    in
    { context }

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] error_pat_not_annotated ~pat =
    let context ~type_of_types:_ ~level:_ ~names:_ ~ok:_ ~error =
      error (CError_typer_pat_not_annotated { pat })
    in
    { context }

  let[@inline always] error_term_var_not_annotated ~var =
    let context ~type_of_types:_ ~level:_ ~names:_ ~ok:_ ~error =
      error (CError_typer_term_var_not_annotated { var })
    in
    { context }

  let[@inline always] error_pat_var_not_annotated ~var =
    let context ~type_of_types:_ ~level:_ ~names:_ ~ok:_ ~error =
      error (CError_typer_pat_var_not_annotated { var })
    in
    { context }

  let[@inline always] error_pairs_not_implemented () =
    let context ~type_of_types:_ ~level:_ ~names:_ ~ok:_ ~error =
      error CError_typer_pairs_not_implemented
    in
    { context }

  let[@inline always] instance ~var =
    let context ~type_of_types:_ ~level ~names ~ok ~error =
      match Name.Tbl.find_opt names var with
      | Some (var_level, type_) ->
          let offset = Level.offset ~from:var_level ~to_:level in
          let type_ = TT_offset { term = type_; offset } in
          ok (offset, type_)
      | None -> error (CError_typer_unknown_var { var })
    in
    { context }

  let[@inline always] with_binder ~var ~type_ f =
    let context ~type_of_types ~level ~names ~ok ~error =
      Name.Tbl.add names var (level, type_);
      let level = Level.next level in
      let { context } = f () in
      let ok value =
        Name.Tbl.remove names var;
        ok value
      in
      context ~type_of_types ~level ~names ~ok ~error
    in
    { context }

  module Unify_context = Unify_context (Normalize)

  let[@inline always] unify_term ~expected ~received =
    let context ~type_of_types:_ ~level:_ ~names:_ ~ok ~error =
      let Unify_context.{ context } = Unify.unify_term ~expected ~received in
      context ~expected_offset:Offset.zero ~received_offset:Offset.zero ~ok
        ~error
    in
    { context }

  let[@inline always] with_tt_loc ~loc f =
    let context ~type_of_types ~level ~names ~ok ~error =
      let { context } = f () in
      let ok term = ok @@ TT_loc { term; loc } in
      let error desc = error @@ CError_loc { error = desc; loc } in
      context ~type_of_types ~level ~names ~ok ~error
    in
    { context }

  let[@inline always] with_tp_loc ~loc f =
    let context ~type_of_types ~level ~names ~ok ~error =
      let { context } =
        f (fun pat k ->
            let pat = TP_loc { pat; loc } in
            k pat)
      in
      context ~type_of_types ~level ~names ~ok ~error
    in
    { context }

  open Ttree

  let[@inline always] tt_type ~type_of_types ~level =
    let offset = Level.offset ~from:type_of_types ~to_:level in
    TT_var { offset }

  let[@inline always] with_normalize_context f =
    let context ~type_of_types:_ ~level:_ ~names:_ ~ok ~error =
      let Normalize_context.{ context } = f () in
      let ok value = ok value in
      let error desc = error desc in
      context ~vars:[] ~offset:Offset.zero ~ok ~error
    in
    { context }

  let[@inline always] normalize_term type_ =
    with_normalize_context @@ fun () -> Normalize.normalize_term type_

  let[@inline always] split_forall type_ =
    let* type_ = normalize_term type_ in
    (* TODO: two normalize guarantees no TT_offset? *)
    (* TODO: it doesn't *)
    let* type_ = normalize_term type_ in
    let context ~type_of_types:_ ~level:_ ~names:_ ~ok ~error =
      match type_ with
      | TT_forall { param; return } -> ok (param, return)
      | TT_var _ | TT_lambda _ | TT_apply _ | TT_annot _ | TT_loc _
      | TT_offset _ ->
          error (Cerror_typer_not_a_forall { type_ })
    in
    { context }

  let[@inline always] tt_annot ~annot term = TT_annot { term; annot }

  let[@inline always] tt_type () =
    let context ~type_of_types ~level ~names:_ ~ok ~error:_ =
      ok @@ tt_type ~type_of_types ~level
    in
    { context }

  let[@inline always] tt_var ~annot ~offset =
    let context ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tt_annot ~annot @@ TT_var { offset }
    in
    { context }

  let[@inline always] tt_forall ~param ~return =
    let context ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ TT_forall { param; return }
    in
    { context }

  let[@inline always] tt_lambda ~param ~return =
    let context ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ TT_lambda { param; return }
    in
    { context }

  let[@inline always] tt_apply ~lambda ~arg =
    let context ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ TT_apply { lambda; arg }
    in
    { context }

  let[@inline always] tt_annot ~term ~annot =
    let context ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tt_annot ~annot term
    in
    { context }

  let[@inline always] tp_annot ~annot pat = TP_annot { pat; annot }

  let[@inline always] tp_var ~annot ~var =
    let context ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ tp_annot ~annot @@ TP_var { var }
    in
    { context }

  let[@inline always] tp_annot ~pat ~annot =
    let context ~type_of_types:_ ~level:_ ~names:_ ~ok ~error:_ =
      ok @@ TP_annot { pat; annot }
    in
    { context }
end
