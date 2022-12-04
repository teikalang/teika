open Ttree

type error = CError of { loc : Location.t; [@opaque] desc : error_desc }

and error_desc =
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
[@@deriving show { with_path = false }]

let[@inline always] repr_hole ~holes ~next_hole_id ~ok ~unknown ~id =
  let index = Uid.repr id in
  let next_hole_index = Uid.repr next_hole_id in
  match index < next_hole_index with
  (* TODO: try unsafe? *)
  | true -> ok (Array.get holes index)
  | false -> unknown ~id

(* TODO: double is a lot *)
let[@inline always] double ~holes =
  let length = Array.length holes in
  let new_holes =
    let length = max (Array.length holes * 2) 1 in
    Array.make length H_open
  in
  Array.blit holes 0 new_holes 0 length;
  new_holes

let[@inline always] create_hole ~holes ~next_hole_id ~ok =
  let hole = next_hole_id in
  let holes =
    match Uid.repr hole >= Array.length holes with
    | true -> double ~holes
    | false -> holes
  in
  let next_hole_id = Uid.next next_hole_id in
  ok ~holes ~next_hole_id hole

module Normalize_context = struct
  type var_info = Subst of { to_ : term } | Bound of { base : Offset.t }

  type 'a normalize_context = {
    (* TODO: accumulate locations during normalization *)
    context :
      'k.
      vars:var_info list ->
      holes:hole array ->
      next_hole_id:Uid.t ->
      offset:Offset.t ->
      ok:('a -> 'k) ->
      error:(error_desc -> 'k) ->
      'k;
  }
  [@@ocaml.unboxed]

  type 'a t = 'a normalize_context

  let[@inline always] test ~loc ~vars ~holes ~next_hole_id ~offset f =
    let { context } = f () in
    let ok value = Ok value in
    let error desc = Error (CError { loc; desc }) in
    context ~vars ~holes ~next_hole_id ~offset ~ok ~error

  let[@inline always] return value =
    let context ~vars:_ ~holes:_ ~next_hole_id:_ ~offset:_ ~ok ~error:_ =
      ok value
    in
    { context }

  let[@inline always] bind context f =
    let { context } = context in
    let context ~vars ~holes ~next_hole_id ~offset ~ok ~error =
      let ok data =
        let { context } = f data in
        context ~vars ~holes ~next_hole_id ~offset ~ok ~error
      in
      context ~vars ~holes ~next_hole_id ~offset ~ok ~error
    in
    { context }

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] repr_var ~var =
    let context ~vars ~holes:_ ~next_hole_id:_ ~offset ~ok ~error:_ =
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
    let context ~vars ~holes ~next_hole_id ~offset ~ok ~error =
      let vars = Bound { base = offset } :: vars in
      let { context } = f () in
      context ~vars ~holes ~next_hole_id ~offset ~ok ~error
    in
    { context }

  let[@inline always] elim_var ~to_ f =
    let context ~vars ~holes ~next_hole_id ~offset ~ok ~error =
      let vars = Subst { to_ } :: vars in
      let { context } = f () in
      context ~vars ~holes ~next_hole_id ~offset ~ok ~error
    in
    { context }

  let[@inline always] with_offset ~offset f =
    let context ~vars ~holes ~next_hole_id ~offset:current_offset ~ok ~error =
      let offset = Offset.(current_offset + offset) in
      let { context } = f () in
      context ~vars ~holes ~next_hole_id ~offset ~ok ~error
    in
    { context }

  let[@inline always] repr_hole ~id =
    let context ~vars:_ ~holes ~next_hole_id ~offset ~ok ~error =
      let unknown ~id = error (CError_normalize_unknown_hole_repr { id }) in
      let ok hole =
        let term =
          match hole with H_open -> TT_hole { id } | H_link { term } -> term
        in

        ok @@ TT_offset { term; offset }
      in
      repr_hole ~holes ~next_hole_id ~ok ~unknown ~id
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
      holes:hole array ->
      next_hole_id:Uid.t ->
      expected_offset:Offset.t ->
      received_offset:Offset.t ->
      ok:(holes:hole array -> next_hole_id:Uid.t -> 'a -> 'k) ->
      error:(holes:hole array -> next_hole_id:Uid.t -> error_desc -> 'k) ->
      'k;
  }
  [@@ocaml.unboxed]

  type 'a t = 'a unify_context

  let[@inline always] test ~loc ~holes ~next_hole_id ~expected_offset
      ~received_offset f =
    let { context } = f () in
    let ok ~holes:_ ~next_hole_id:_ value = Ok value in
    let error ~holes:_ ~next_hole_id:_ desc = Error (CError { loc; desc }) in
    context ~holes ~next_hole_id ~expected_offset ~received_offset ~ok ~error

  let[@inline always] return value =
    let context ~holes ~next_hole_id ~expected_offset:_ ~received_offset:_ ~ok
        ~error:_ =
      ok ~holes ~next_hole_id value
    in
    { context }

  let[@inline always] bind context f =
    let { context } = context in
    let context ~holes ~next_hole_id ~expected_offset ~received_offset ~ok
        ~error =
      let ok ~holes ~next_hole_id data =
        let { context } = f data in
        context ~holes ~next_hole_id ~expected_offset ~received_offset ~ok
          ~error
      in
      context ~holes ~next_hole_id ~expected_offset ~received_offset ~ok ~error
    in
    { context }

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] error_var_clash ~expected ~received =
    let context ~holes ~next_hole_id ~expected_offset:_ ~received_offset:_ ~ok:_
        ~error =
      error ~holes ~next_hole_id (CError_unify_var_clash { expected; received })
    in
    { context }

  let[@inline always] error_type_clash ~expected ~received =
    let context ~holes ~next_hole_id ~expected_offset:_ ~received_offset:_ ~ok:_
        ~error =
      error ~holes ~next_hole_id
        (CError_unify_type_clash { expected; received })
    in
    { context }

  let[@inline always] error_pat_clash ~expected ~received =
    let context ~holes ~next_hole_id ~expected_offset:_ ~received_offset:_ ~ok:_
        ~error =
      error ~holes ~next_hole_id (CError_unify_pat_clash { expected; received })
    in
    { context }

  let[@inline always] error_var_escape_scope ~var =
    let context ~holes ~next_hole_id ~expected_offset:_ ~received_offset:_ ~ok:_
        ~error =
      error ~holes ~next_hole_id (CError_unify_var_escape_scope { var })
    in
    { context }

  let[@inline always] with_normalize_context f =
    let context ~holes ~next_hole_id ~expected_offset:_ ~received_offset:_ ~ok
        ~error =
      let Normalize_context.{ context } = f () in
      let offset = Offset.zero in
      let ok value = ok ~holes ~next_hole_id value in
      let error desc = error ~holes ~next_hole_id desc in
      context ~vars:[] ~holes ~next_hole_id ~offset ~ok ~error
    in
    { context }

  let[@inline always] normalize_term term =
    with_normalize_context @@ fun () -> Normalize.normalize_term term

  let[@inline always] expected_offset () =
    let context ~holes ~next_hole_id ~expected_offset ~received_offset:_ ~ok
        ~error:_ =
      ok ~holes ~next_hole_id expected_offset
    in
    { context }

  let[@inline always] received_offset () =
    let context ~holes ~next_hole_id ~expected_offset:_ ~received_offset ~ok
        ~error:_ =
      ok ~holes ~next_hole_id received_offset
    in
    { context }

  let[@inline always] with_expected_offset ~offset f =
    let context ~holes ~next_hole_id ~expected_offset ~received_offset ~ok
        ~error =
      let expected_offset = Offset.(expected_offset + offset) in
      let { context } = f () in
      context ~holes ~next_hole_id ~expected_offset ~received_offset ~ok ~error
    in
    { context }

  let[@inline always] with_received_offset ~offset f =
    let context ~holes ~next_hole_id ~expected_offset ~received_offset ~ok
        ~error =
      let received_offset = Offset.(received_offset + offset) in
      let { context } = f () in
      context ~holes ~next_hole_id ~expected_offset ~received_offset ~ok ~error
    in
    { context }

  let[@inline always] repr_hole ~id =
    let context ~holes ~next_hole_id ~expected_offset:_ ~received_offset:_ ~ok
        ~error =
      let unknown ~id =
        error ~holes ~next_hole_id (CError_unify_unknown_hole { id })
      in
      let ok repr = ok ~holes ~next_hole_id repr in
      repr_hole ~holes ~next_hole_id ~ok ~unknown ~id
    in
    { context }

  let[@inline always] link_hole ~id ~to_ ~offset =
    let context ~holes ~next_hole_id ~expected_offset:_ ~received_offset:_ ~ok
        ~error =
      let index = Uid.repr id in
      let next_hole_index = Uid.repr next_hole_id in
      match index < next_hole_index with
      | true -> (
          match Array.get holes index with
          | H_open ->
              let term = TT_offset { term = to_; offset } in
              Array.set holes index (H_link { term });
              ok ~holes ~next_hole_id ()
          | H_link { term = _ } ->
              error ~holes ~next_hole_id (CError_unify_linked_hole { id }))
      | false -> error ~holes ~next_hole_id (CError_unify_unknown_hole { id })
    in
    { context }

  (* TODO: diff is a bad name *)
  let[@inline always] lower_hole ~id ~diff =
    let context ~holes ~next_hole_id ~expected_offset:_ ~received_offset:_ ~ok
        ~error =
      let index = Uid.repr id in
      let next_hole_index = Uid.repr next_hole_id in
      match index < next_hole_index with
      | true -> (
          match Array.get holes index with
          | H_open -> (
              match Offset.(diff < zero) with
              | true ->
                  error ~holes ~next_hole_id
                    (CError_unify_lower_to_higher { id })
              | false ->
                  let ok ~holes ~next_hole_id new_hole =
                    let new_hole = TT_hole { id = new_hole } in
                    let term = TT_offset { term = new_hole; offset = diff } in
                    Array.set holes index (H_link { term });
                    ok ~holes ~next_hole_id ()
                  in
                  create_hole ~holes ~next_hole_id ~ok)
          | H_link { term = _ } ->
              error ~holes ~next_hole_id (CError_unify_linked_hole { id }))
      | false -> error ~holes ~next_hole_id (CError_unify_unknown_hole { id })
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
      holes:hole array ->
      next_hole_id:Uid.t ->
      type_of_types:Level.t ->
      level:Level.t ->
      names:(Level.t * term) Name.Tbl.t ->
      ok:(holes:hole array -> next_hole_id:Uid.t -> 'a -> 'k) ->
      error:(holes:hole array -> next_hole_id:Uid.t -> error_desc -> 'k) ->
      'k;
  }
  [@@ocaml.unboxed]

  type 'a t = 'a typer_context

  let[@inline always] run ~loc f =
    let { context } = f () in
    (* TODO: meaningful size? *)
    let holes = Array.make 1024 H_open in
    let next_hole_id = Uid.initial in
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
    let ok ~holes:_ ~next_hole_id:_ value = Ok value in
    let error ~holes:_ ~next_hole_id:_ desc = Error (CError { loc; desc }) in
    context ~holes ~next_hole_id ~type_of_types ~level ~names ~ok ~error

  let[@inline always] test ~loc ~holes ~next_hole_id ~type_of_types ~level
      ~names f =
    let { context } = f () in
    let ok ~holes:_ ~next_hole_id:_ value = Ok value in
    let error ~holes:_ ~next_hole_id:_ desc = Error (CError { loc; desc }) in
    context ~holes ~next_hole_id ~type_of_types ~level ~names ~ok ~error

  let[@inline always] return value =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error:_ =
      ok ~holes ~next_hole_id value
    in
    { context }

  let[@inline always] bind context f =
    let { context } = context in
    let context ~holes ~next_hole_id ~type_of_types ~level ~names ~ok ~error =
      let ok ~holes ~next_hole_id data =
        let { context } = f data in
        context ~holes ~next_hole_id ~type_of_types ~level ~names ~ok ~error
      in
      context ~holes ~next_hole_id ~type_of_types ~level ~names ~ok ~error
    in
    { context }

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] error_pat_not_annotated ~pat =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok:_
        ~error =
      error ~holes ~next_hole_id (CError_typer_pat_not_annotated { pat })
    in
    { context }

  let[@inline always] error_pat_not_pair ~pat ~expected =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok:_
        ~error =
      error ~holes ~next_hole_id (CError_typer_pat_not_pair { pat; expected })
    in
    { context }

  let[@inline always] error_term_var_not_annotated ~var =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok:_
        ~error =
      error ~holes ~next_hole_id (CError_typer_term_var_not_annotated { var })
    in
    { context }

  let[@inline always] error_term_hole_not_annotated ~hole =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok:_
        ~error =
      error ~holes ~next_hole_id (CError_typer_term_hole_not_annotated { hole })
    in
    { context }

  let[@inline always] error_pat_var_not_annotated ~var =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok:_
        ~error =
      error ~holes ~next_hole_id (CError_typer_pat_var_not_annotated { var })
    in
    { context }

  let[@inline always] instance ~var =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level ~names ~ok ~error =
      match Name.Tbl.find_opt names var with
      | Some (var_level, type_) ->
          let offset = Level.offset ~from:var_level ~to_:level in
          let type_ = TT_offset { term = type_; offset } in
          ok ~holes ~next_hole_id (offset, type_)
      | None -> error ~holes ~next_hole_id (CError_typer_unknown_var { var })
    in
    { context }

  let[@inline always] with_binder ~var ~type_ f =
    let context ~holes ~next_hole_id ~type_of_types ~level ~names ~ok ~error =
      Name.Tbl.add names var (level, type_);
      let level = Level.next level in
      let { context } = f () in
      let ok ~holes ~next_hole_id value =
        Name.Tbl.remove names var;
        ok ~holes ~next_hole_id value
      in
      context ~holes ~next_hole_id ~type_of_types ~level ~names ~ok ~error
    in
    { context }

  module Unify_context = Unify_context (Normalize)

  let[@inline always] unify_term ~expected ~received =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error =
      let Unify_context.{ context } = Unify.unify_term ~expected ~received in
      context ~holes ~next_hole_id ~expected_offset:Offset.zero
        ~received_offset:Offset.zero ~ok ~error
    in
    { context }

  let[@inline always] with_tt_loc ~loc f =
    let context ~holes ~next_hole_id ~type_of_types ~level ~names ~ok ~error =
      let { context } = f () in
      let ok ~holes ~next_hole_id term =
        ok ~holes ~next_hole_id @@ TT_loc { term; loc }
      in
      context ~holes ~next_hole_id ~type_of_types ~level ~names ~ok ~error
    in
    { context }

  let[@inline always] with_tp_loc ~loc f =
    let context ~holes ~next_hole_id ~type_of_types ~level ~names ~ok ~error =
      let { context } =
        f (fun pat k ->
            let pat = TP_loc { pat; loc } in
            k pat)
      in
      context ~holes ~next_hole_id ~type_of_types ~level ~names ~ok ~error
    in
    { context }

  open Ttree

  let[@inline always] tt_type ~type_of_types ~level =
    let offset = Level.offset ~from:type_of_types ~to_:level in
    TT_var { offset }

  let[@inline always] with_normalize_context f =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error =
      let Normalize_context.{ context } = f () in
      let ok value = ok ~holes ~next_hole_id value in
      let error desc = error ~holes ~next_hole_id desc in
      context ~vars:[] ~holes ~next_hole_id ~offset:Offset.zero ~ok ~error
    in
    { context }

  let[@inline always] normalize_term type_ =
    with_normalize_context @@ fun () -> Normalize.normalize_term type_

  let[@inline always] split_forall type_ =
    let* type_ = normalize_term type_ in
    (* TODO: two normalize guarantees no TT_offset? *)
    (* TODO: it doesn't *)
    let* type_ = normalize_term type_ in
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error =
      match type_ with
      | TT_forall { param; return } -> ok ~holes ~next_hole_id (param, return)
      | TT_var _ | TT_hole _ | TT_lambda _ | TT_apply _ | TT_exists _
      | TT_pair _ | TT_let _ | TT_annot _ | TT_loc _ | TT_offset _ ->
          error ~holes ~next_hole_id (Cerror_typer_not_a_forall { type_ })
    in
    { context }

  let[@inline always] split_exists type_ =
    let* type_ = normalize_term type_ in
    (* TODO: two normalize guarantees no TT_offset? *)
    let* type_ = normalize_term type_ in
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error =
      match type_ with
      | TT_exists { left; right } -> ok ~holes ~next_hole_id (left, right)
      | TT_var _ | TT_hole _ | TT_forall _ | TT_lambda _ | TT_apply _
      | TT_pair _ | TT_let _ | TT_annot _ | TT_loc _ | TT_offset _ ->
          error ~holes ~next_hole_id (Cerror_typer_not_an_exists { type_ })
    in
    { context }

  let[@inline always] tt_annot ~annot term = TT_annot { term; annot }

  let[@inline always] tt_type () =
    let context ~holes ~next_hole_id ~type_of_types ~level ~names:_ ~ok ~error:_
        =
      ok ~holes ~next_hole_id @@ tt_type ~type_of_types ~level
    in
    { context }

  let[@inline always] tt_var ~annot ~offset =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error:_ =
      ok ~holes ~next_hole_id @@ tt_annot ~annot @@ TT_var { offset }
    in
    { context }

  let[@inline always] tt_forall ~param ~return =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error:_ =
      ok ~holes ~next_hole_id @@ TT_forall { param; return }
    in
    { context }

  let[@inline always] tt_lambda ~param ~return =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error:_ =
      ok ~holes ~next_hole_id @@ TT_lambda { param; return }
    in
    { context }

  let[@inline always] tt_apply ~lambda ~arg =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error:_ =
      ok ~holes ~next_hole_id @@ TT_apply { lambda; arg }
    in
    { context }

  let[@inline always] tt_exists ~left ~right =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error:_ =
      ok ~holes ~next_hole_id @@ TT_exists { left; right }
    in
    { context }

  let[@inline always] tt_pair ~left ~right =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error:_ =
      ok ~holes ~next_hole_id @@ TT_pair { left; right }
    in
    { context }

  let[@inline always] tt_let ~bound ~return =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error:_ =
      ok ~holes ~next_hole_id @@ TT_let { bound; return }
    in
    { context }

  let[@inline always] tt_annot ~term ~annot =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error:_ =
      ok ~holes ~next_hole_id @@ tt_annot ~annot term
    in
    { context }

  let[@inline always] tp_annot ~annot pat = TP_annot { pat; annot }

  let[@inline always] tp_var ~annot ~var =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error:_ =
      ok ~holes ~next_hole_id @@ tp_annot ~annot @@ TP_var { var }
    in
    { context }

  let[@inline always] tp_pair ~left ~right =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error:_ =
      ok ~holes ~next_hole_id @@ TP_pair { left; right }
    in
    { context }

  let[@inline always] tp_annot ~pat ~annot =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error:_ =
      ok ~holes ~next_hole_id @@ TP_annot { pat; annot }
    in
    { context }

  let[@inline always] tannot ~loc ~pat ~annot =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error:_ =
      ok ~holes ~next_hole_id @@ TAnnot { loc; pat; annot }
    in
    { context }

  let[@inline always] tbind ~loc ~pat ~value =
    let context ~holes ~next_hole_id ~type_of_types:_ ~level:_ ~names:_ ~ok
        ~error:_ =
      ok ~holes ~next_hole_id @@ TBind { loc; pat; value }
    in
    { context }
end
