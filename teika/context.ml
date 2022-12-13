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
  | CError_typer_unknown_var of { name : Name.t }
  | Cerror_typer_not_a_forall of { type_ : term [@printer Tprinter.pp_term] }
  | CError_typer_pat_not_annotated of { pat : Ltree.pat }
  | CError_typer_pairs_not_implemented
  (* invariant *)
  | CError_typer_term_var_not_annotated of { var : Offset.t }
  | CError_typer_pat_var_not_annotated of { name : Name.t }
[@@deriving show { with_path = false }]

type ('a, 'b) result = { match_ : 'k. ok:('a -> 'k) -> error:('b -> 'k) -> 'k }
[@@ocaml.unboxed]

let[@inline always] ok value = { match_ = (fun ~ok ~error:_ -> ok value) }
let[@inline always] error desc = { match_ = (fun ~ok:_ ~error -> error desc) }

type var_info = Subst of { to_ : term } | Bound of { base : Offset.t }

module Normalize_context = struct
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

module Unify_context = struct
  type 'a unify_context =
    expected_vars:var_info list ->
    expected_offset:Offset.t ->
    received_vars:var_info list ->
    received_offset:Offset.t ->
    ('a, error) result

  type 'a t = 'a unify_context

  let[@inline always] test ~expected_vars ~expected_offset ~received_vars
      ~received_offset f =
    (f () ~expected_vars ~expected_offset ~received_vars ~received_offset)
      .match_
      ~ok:(fun value -> Ok value)
      ~error:(fun desc -> Error desc)

  let[@inline always] return_raw value ~expected_vars:_ ~expected_offset:_
      ~received_vars:_ ~received_offset:_ =
    value

  let[@inline always] return value = return_raw @@ ok value
  let[@inline always] fail desc = return_raw @@ error desc

  let[@inline always] bind context f ~expected_vars ~expected_offset
      ~received_vars ~received_offset =
    (context ~expected_vars ~expected_offset ~received_vars ~received_offset)
      .match_
      ~ok:(fun value ->
        f value ~expected_vars ~expected_offset ~received_vars ~received_offset)
      ~error

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] error_var_clash ~expected ~received =
    fail @@ CError_unify_var_clash { expected; received }

  let[@inline always] error_type_clash ~expected ~received =
    fail @@ CError_unify_type_clash { expected; received }

  let[@inline always] error_pat_clash ~expected ~received =
    fail @@ CError_unify_pat_clash { expected; received }

  let[@inline always] error_var_escape_scope ~var =
    fail @@ CError_unify_var_escape_scope { var }

  let[@inline always] with_expected_normalize_context f ~expected_vars
      ~expected_offset ~received_vars:_ ~received_offset:_ =
    f () ~vars:expected_vars ~offset:expected_offset

  let[@inline always] with_received_normalize_context f ~expected_vars:_
      ~expected_offset:_ ~received_vars ~received_offset =
    f () ~vars:received_vars ~offset:received_offset

  let[@inline always] expected_offset () ~expected_vars:_ ~expected_offset
      ~received_vars:_ ~received_offset:_ =
    ok expected_offset

  let[@inline always] received_offset () ~expected_vars:_ ~expected_offset:_
      ~received_vars:_ ~received_offset =
    ok received_offset

  let[@inline always] with_expected_offset ~offset f ~expected_vars
      ~expected_offset ~received_vars ~received_offset =
    let expected_offset = Offset.(expected_offset + offset) in
    f () ~expected_vars ~expected_offset ~received_vars ~received_offset

  let[@inline always] with_received_offset ~offset f ~expected_vars
      ~expected_offset ~received_vars ~received_offset =
    let received_offset = Offset.(received_offset + offset) in
    f () ~expected_vars ~expected_offset ~received_vars ~received_offset
end

module Typer_context = struct
  type 'a typer_context =
    type_of_types:Level.t ->
    level:Level.t ->
    (* TODO: Hashtbl *)
    names:(Level.t * term) Name.Map.t ->
    received_vars:var_info list ->
    ('a, error) result

  type 'a t = 'a typer_context

  let[@inline always] run f =
    let type_of_types = Level.zero in
    let level = Level.next type_of_types in
    let names = Name.Map.empty in
    let type_name = Name.make "Type" in
    let names =
      let type_ = TT_var { offset = Offset.zero } in
      let type_ = TT_annot { term = type_; annot = type_ } in
      (* TODO: better place for constants *)
      Name.Map.add type_name (type_of_types, type_) names
    in
    let received_vars = [ Bound { base = Offset.zero } ] in
    (* TODO: should Type be here? *)
    (f () ~type_of_types ~level ~names ~received_vars).match_
      ~ok:(fun value -> Ok value)
      ~error:(fun desc -> Error desc)

  let[@inline always] test ~type_of_types ~level ~names ~received_vars f =
    (f () ~type_of_types ~level ~names ~received_vars).match_
      ~ok:(fun value -> Ok value)
      ~error:(fun desc -> Error desc)

  let[@inline always] return_raw value ~type_of_types:_ ~level:_ ~names:_
      ~received_vars:_ =
    value

  let[@inline always] return value = return_raw @@ ok value
  let[@inline always] fail desc = return_raw @@ error desc

  let[@inline always] bind context f ~type_of_types ~level ~names ~received_vars
      =
    (context ~type_of_types ~level ~names ~received_vars).match_
      ~ok:(fun value -> f value ~type_of_types ~level ~names ~received_vars)
      ~error

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] error_pat_not_annotated ~pat =
    fail @@ CError_typer_pat_not_annotated { pat }

  let[@inline always] error_term_var_not_annotated ~var =
    fail @@ CError_typer_term_var_not_annotated { var }

  let[@inline always] error_pat_var_not_annotated ~name =
    fail @@ CError_typer_pat_var_not_annotated { name }

  let[@inline always] error_pairs_not_implemented () =
    fail @@ CError_typer_pairs_not_implemented

  let[@inline always] error_not_a_forall ~type_ =
    fail @@ Cerror_typer_not_a_forall { type_ }

  let[@inline always] instance ~name ~type_of_types:_ ~level ~names
      ~received_vars:_ =
    match Name.Map.find_opt name names with
    | Some (var_level, type_) ->
        let offset = Level.offset ~from:var_level ~to_:level in
        let type_ = TT_offset { term = type_; offset } in
        ok @@ (offset, type_)
    | None -> error @@ CError_typer_unknown_var { name }

  let[@inline always] with_binder ~name ~type_ f ~type_of_types ~level ~names
      ~received_vars =
    let names = Name.Map.add name (level, type_) names in
    let received_vars = Bound { base = Offset.zero } :: received_vars in
    (* TODO: weird, level increases first? *)
    let level = Level.next level in
    f () ~type_of_types ~level ~names ~received_vars

  let[@inline always] with_unify_context f ~type_of_types:_ ~level:_ ~names:_
      ~received_vars:_ =
    f () ~expected_vars:[] ~expected_offset:Offset.zero ~received_vars:[]
      ~received_offset:Offset.zero

  let[@inline always] with_received_normalize_context f ~type_of_types:_
      ~level:_ ~names:_ ~received_vars:_ =
    f () ~vars:[] ~offset:Offset.zero

  let[@inline always] with_tt_loc ~loc f ~type_of_types ~level ~names
      ~received_vars =
    (f () ~type_of_types ~level ~names ~received_vars).match_
      ~ok:(fun term -> ok @@ TT_loc { term; loc })
      ~error:(fun desc -> error @@ CError_loc { error = desc; loc })

  let[@inline always] with_tp_loc ~loc f ~type_of_types ~level ~names
      ~received_vars =
    f
      (fun pat k ->
        let pat = TP_loc { pat; loc } in
        k pat)
      ~type_of_types ~level ~names ~received_vars

  open Ttree

  let[@inline always] tt_type ~type_of_types ~level =
    let offset = Level.offset ~from:type_of_types ~to_:level in
    TT_var { offset }

  let[@inline always] tt_annot ~annot term = TT_annot { term; annot }

  let[@inline always] tt_type () ~type_of_types ~level ~names:_ ~received_vars:_
      =
    ok @@ tt_type ~type_of_types ~level

  let[@inline always] tt_var ~annot ~offset =
    return @@ tt_annot ~annot @@ TT_var { offset }

  let[@inline always] tt_forall ~param ~return =
    return_raw @@ ok @@ TT_forall { param; return }

  let[@inline always] tt_lambda ~param ~return =
    return_raw @@ ok @@ TT_lambda { param; return }

  let[@inline always] tt_apply ~lambda ~arg = return @@ TT_apply { lambda; arg }
  let[@inline always] tt_annot ~term ~annot = return @@ tt_annot ~annot term
  let[@inline always] tp_annot ~annot pat = TP_annot { pat; annot }

  let[@inline always] tp_var ~annot ~var =
    return @@ tp_annot ~annot @@ TP_var { var }

  let[@inline always] tp_annot ~pat ~annot = return @@ TP_annot { pat; annot }
end
