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

type ('a, 'b) result = { match_ : 'k. ok:('a -> 'k) -> error:('b -> 'k) -> 'k }
[@@ocaml.unboxed]

let[@inline always] ok value = { match_ = (fun ~ok ~error:_ -> ok value) }
let[@inline always] error desc = { match_ = (fun ~ok:_ ~error -> error desc) }

module Normalize_context = struct
  type var_info = Subst of { to_ : term } | Bound of { base : Offset.t }

  type 'a normalize_context =
    vars:var_info list -> offset:Offset.t -> ('a, error) result

  type 'a t = 'a normalize_context

  let[@inline always] test ~vars ~offset f =
    (f () ~vars ~offset).match_
      ~ok:(fun value -> Ok value)
      ~error:(fun desc -> Error desc)

  let[@inline always] return value ~vars:_ ~offset:_ = ok value

  let[@inline always] bind context f ~vars ~offset =
    (context ~vars ~offset).match_
      ~ok:(fun value -> f value ~vars ~offset)
      ~error

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] repr_var ~var ~vars ~offset =
    match
      (* TODO: should this be var + offset? *)
      let index = Offset.(repr (var - one)) in
      List.nth_opt vars index
    with
    | Some (Subst { to_ }) ->
        let offset = Offset.(var + offset) in
        ok @@ TT_offset { term = to_; offset }
    | Some (Bound { base }) ->
        let offset = Offset.(var + offset - base) in
        ok @@ TT_var { offset }
    | None | (exception Invalid_argument _) ->
        (* free var *)
        let offset = Offset.(var + offset) in
        ok @@ TT_var { offset }

  let[@inline always] with_var f ~vars ~offset =
    let vars = Bound { base = offset } :: vars in
    f () ~vars ~offset

  let[@inline always] elim_var ~to_ f ~vars ~offset =
    let vars = Subst { to_ } :: vars in
    f () ~vars ~offset

  let[@inline always] with_offset ~offset f ~vars ~offset:current_offset =
    let offset = Offset.(current_offset + offset) in
    f () ~vars ~offset
end

module Unify_context (Normalize : sig
  val normalize_term : term -> term Normalize_context.t
end) =
struct
  type 'a unify_context =
    expected_offset:Offset.t -> received_offset:Offset.t -> ('a, error) result

  type 'a t = 'a unify_context

  let[@inline always] test ~expected_offset ~received_offset f =
    (f () ~expected_offset ~received_offset).match_
      ~ok:(fun value -> Ok value)
      ~error:(fun desc -> Error desc)

  let[@inline always] return value ~expected_offset:_ ~received_offset:_ =
    ok value

  let[@inline always] bind context f ~expected_offset ~received_offset =
    (context ~expected_offset ~received_offset).match_
      ~ok:(fun value -> f value ~expected_offset ~received_offset)
      ~error

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] error_var_clash ~expected ~received ~expected_offset:_
      ~received_offset:_ =
    error @@ CError_unify_var_clash { expected; received }

  let[@inline always] error_type_clash ~expected ~received ~expected_offset:_
      ~received_offset:_ =
    error @@ CError_unify_type_clash { expected; received }

  let[@inline always] error_pat_clash ~expected ~received ~expected_offset:_
      ~received_offset:_ =
    error @@ CError_unify_pat_clash { expected; received }

  let[@inline always] error_var_escape_scope ~var ~expected_offset:_
      ~received_offset:_ =
    error @@ CError_unify_var_escape_scope { var }

  let[@inline always] with_normalize_context f ~expected_offset:_
      ~received_offset:_ =
    f () ~vars:[] ~offset:Offset.zero

  let[@inline always] normalize_term term =
    with_normalize_context @@ fun () -> Normalize.normalize_term term

  let[@inline always] expected_offset () ~expected_offset ~received_offset:_ =
    ok expected_offset

  let[@inline always] received_offset () ~expected_offset:_ ~received_offset =
    ok received_offset

  let[@inline always] with_expected_offset ~offset f ~expected_offset
      ~received_offset =
    let expected_offset = Offset.(expected_offset + offset) in
    f () ~expected_offset ~received_offset

  let[@inline always] with_received_offset ~offset f ~expected_offset
      ~received_offset =
    let received_offset = Offset.(received_offset + offset) in
    f () ~expected_offset ~received_offset
end

module Typer_context (Normalize : sig
  val normalize_term : term -> term Normalize_context.t
end) (Unify : sig
  val unify_term :
    expected:term -> received:term -> unit Unify_context(Normalize).t
end) =
struct
  type 'a typer_context =
    type_of_types:Level.t ->
    level:Level.t ->
    names:(Level.t * term) Name.Tbl.t ->
    ('a, error) result

  type 'a t = 'a typer_context

  let[@inline always] run f =
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
    (f () ~type_of_types ~level ~names).match_
      ~ok:(fun value -> Ok value)
      ~error:(fun desc -> Error desc)

  let[@inline always] test ~type_of_types ~level ~names f =
    (f () ~type_of_types ~level ~names).match_
      ~ok:(fun value -> Ok value)
      ~error:(fun desc -> Error desc)

  let[@inline always] return value ~type_of_types:_ ~level:_ ~names:_ = ok value

  let[@inline always] bind context f ~type_of_types ~level ~names =
    (context ~type_of_types ~level ~names).match_
      ~ok:(fun value -> f value ~type_of_types ~level ~names)
      ~error

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] error_pat_not_annotated ~pat ~type_of_types:_ ~level:_
      ~names:_ =
    error @@ CError_typer_pat_not_annotated { pat }

  let[@inline always] error_term_var_not_annotated ~var ~type_of_types:_
      ~level:_ ~names:_ =
    error @@ CError_typer_term_var_not_annotated { var }

  let[@inline always] error_pat_var_not_annotated ~var ~type_of_types:_ ~level:_
      ~names:_ =
    error @@ CError_typer_pat_var_not_annotated { var }

  let[@inline always] error_pairs_not_implemented () ~type_of_types:_ ~level:_
      ~names:_ =
    error @@ CError_typer_pairs_not_implemented

  let[@inline always] instance ~var ~type_of_types:_ ~level ~names =
    match Name.Tbl.find_opt names var with
    | Some (var_level, type_) ->
        let offset = Level.offset ~from:var_level ~to_:level in
        let type_ = TT_offset { term = type_; offset } in
        ok @@ (offset, type_)
    | None -> error @@ CError_typer_unknown_var { var }

  let[@inline always] with_binder ~var ~type_ f ~type_of_types ~level ~names =
    Name.Tbl.add names var (level, type_);
    let level = Level.next level in
    (f () ~type_of_types ~level ~names).match_
      ~ok:(fun value ->
        Name.Tbl.remove names var;
        ok value)
      ~error

  let[@inline always] unify_term ~expected ~received ~type_of_types:_ ~level:_
      ~names:_ =
    Unify.unify_term ~expected ~received ~expected_offset:Offset.zero
      ~received_offset:Offset.zero

  let[@inline always] with_tt_loc ~loc f ~type_of_types ~level ~names =
    (f () ~type_of_types ~level ~names).match_
      ~ok:(fun term -> ok @@ TT_loc { term; loc })
      ~error:(fun desc -> error @@ CError_loc { error = desc; loc })

  let[@inline always] with_tp_loc ~loc f ~type_of_types ~level ~names =
    f
      (fun pat k ->
        let pat = TP_loc { pat; loc } in
        k pat)
      ~type_of_types ~level ~names

  open Ttree

  let[@inline always] tt_type ~type_of_types ~level =
    let offset = Level.offset ~from:type_of_types ~to_:level in
    TT_var { offset }

  let[@inline always] with_normalize_context f ~type_of_types:_ ~level:_
      ~names:_ =
    f () ~vars:[] ~offset:Offset.zero

  let[@inline always] normalize_term type_ =
    with_normalize_context @@ fun () -> Normalize.normalize_term type_

  let[@inline always] split_forall type_ =
    let* type_ = normalize_term type_ in
    (* TODO: two normalize guarantees no TT_offset? *)
    (* TODO: it doesn't *)
    let* type_ = normalize_term type_ in
    fun ~type_of_types:_ ~level:_ ~names:_ ->
      match type_ with
      | TT_forall { param; return } -> ok @@ (param, return)
      | TT_var _ | TT_lambda _ | TT_apply _ | TT_annot _ | TT_loc _
      | TT_offset _ ->
          error @@ Cerror_typer_not_a_forall { type_ }

  let[@inline always] tt_annot ~annot term = TT_annot { term; annot }

  let[@inline always] tt_type () ~type_of_types ~level ~names:_ =
    ok @@ tt_type ~type_of_types ~level

  let[@inline always] tt_var ~annot ~offset ~type_of_types:_ ~level:_ ~names:_ =
    ok @@ tt_annot ~annot @@ TT_var { offset }

  let[@inline always] tt_forall ~param ~return ~type_of_types:_ ~level:_
      ~names:_ =
    ok @@ TT_forall { param; return }

  let[@inline always] tt_lambda ~param ~return ~type_of_types:_ ~level:_
      ~names:_ =
    ok @@ TT_lambda { param; return }

  let[@inline always] tt_apply ~lambda ~arg ~type_of_types:_ ~level:_ ~names:_ =
    ok @@ TT_apply { lambda; arg }

  let[@inline always] tt_annot ~term ~annot ~type_of_types:_ ~level:_ ~names:_ =
    ok @@ tt_annot ~annot term

  let[@inline always] tp_annot ~annot pat = TP_annot { pat; annot }

  let[@inline always] tp_var ~annot ~var ~type_of_types:_ ~level:_ ~names:_ =
    ok @@ tp_annot ~annot @@ TP_var { var }

  let[@inline always] tp_annot ~pat ~annot ~type_of_types:_ ~level:_ ~names:_ =
    ok @@ TP_annot { pat; annot }
end
