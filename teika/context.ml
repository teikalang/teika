open Ttree

type error =
  (* TODO: why track nested locations?
      Probably because things like macros exists *)
  | CError_loc of { error : error; loc : Location.t [@opaque] }
  (* unify *)
  | CError_unify_var_clash of { expected : Offset.t; received : Offset.t }
  | CError_unify_type_clash of {
      expected : ex_term; [@printer Tprinter.pp_ex_term]
      received : ex_term; [@printer Tprinter.pp_ex_term]
    }
  | CError_unify_pat_clash of {
      expected : ex_pat; [@printer Tprinter.pp_ex_pat]
      received : ex_pat; [@printer Tprinter.pp_ex_pat]
    }
  | CError_unify_var_escape_scope of { var : Offset.t }
  (* typer *)
  | CError_typer_unknown_var of { name : Name.t }
  | Cerror_typer_not_a_forall of {
      type_ : ex_term; [@printer Tprinter.pp_ex_term]
    }
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

type var_info = Bound

module Normalize_context = struct
  type 'a normalize_context = vars:var_info list -> ('a, error) result
  type 'a t = 'a normalize_context

  let[@inline always] test ~vars f =
    (f () ~vars).match_
      ~ok:(fun value -> Ok value)
      ~error:(fun desc -> Error desc)

  let[@inline always] return value ~vars:_ = ok value

  let[@inline always] bind context f ~vars =
    (context ~vars).match_ ~ok:(fun value -> f value ~vars) ~error

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] with_var f ~vars =
    let vars = Bound :: vars in
    f () ~vars
end

module Unify_context = struct
  type 'a unify_context =
    expected_vars:var_info list ->
    received_vars:var_info list ->
    ('a, error) result

  type 'a t = 'a unify_context

  let[@inline always] test ~expected_vars ~received_vars f =
    (f () ~expected_vars ~received_vars).match_
      ~ok:(fun value -> Ok value)
      ~error:(fun desc -> Error desc)

  let[@inline always] return_raw value ~expected_vars:_ ~received_vars:_ = value
  let[@inline always] return value = return_raw @@ ok value
  let[@inline always] fail desc = return_raw @@ error desc

  let[@inline always] bind context f ~expected_vars ~received_vars =
    (context ~expected_vars ~received_vars).match_
      ~ok:(fun value -> f value ~expected_vars ~received_vars)
      ~error

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] error_var_clash ~expected ~received =
    fail @@ CError_unify_var_clash { expected; received }

  let[@inline always] error_type_clash ~expected ~received =
    let expected = Ex_term expected in
    let received = Ex_term received in
    fail @@ CError_unify_type_clash { expected; received }

  let[@inline always] error_pat_clash ~expected ~received =
    let expected = Ex_pat expected in
    let received = Ex_pat received in
    fail @@ CError_unify_pat_clash { expected; received }

  let[@inline always] error_var_escape_scope ~var =
    fail @@ CError_unify_var_escape_scope { var }

  let[@inline always] with_expected_normalize_context f ~expected_vars
      ~received_vars:_ =
    f () ~vars:expected_vars

  let[@inline always] with_received_normalize_context f ~expected_vars:_
      ~received_vars =
    f () ~vars:received_vars
end

module Typer_context = struct
  type 'a typer_context =
    type_of_types:Level.t ->
    level:Level.t ->
    (* TODO: Hashtbl *)
    names:(Level.t * ex_term) Name.Map.t ->
    expected_vars:var_info list ->
    received_vars:var_info list ->
    ('a, error) result

  type 'a t = 'a typer_context

  let[@inline always] run f =
    let level = Level.next Level.zero in
    let type_of_types = level in
    let names = Name.Map.empty in
    let type_name = Name.make "Type" in
    let names =
      let type_ = TT_var { offset = Offset.zero } in
      let type_ = TT_annot { term = type_; annot = type_ } in
      (* TODO: better place for constants *)
      Name.Map.add type_name (type_of_types, Ex_term type_) names
    in
    let received_vars = [ Bound ] in
    let expected_vars = received_vars in
    (* TODO: should Type be here? *)
    (f () ~type_of_types ~level ~names ~expected_vars ~received_vars).match_
      ~ok:(fun value -> Ok value)
      ~error:(fun desc -> Error desc)

  let[@inline always] test ~type_of_types ~level ~names ~expected_vars
      ~received_vars f =
    (f () ~type_of_types ~level ~names ~expected_vars ~received_vars).match_
      ~ok:(fun value -> Ok value)
      ~error:(fun desc -> Error desc)

  let[@inline always] return_raw value ~type_of_types:_ ~level:_ ~names:_
      ~expected_vars:_ ~received_vars:_ =
    value

  let[@inline always] return value = return_raw @@ ok value
  let[@inline always] fail desc = return_raw @@ error desc

  let[@inline always] bind context f ~type_of_types ~level ~names ~expected_vars
      ~received_vars =
    (context ~type_of_types ~level ~names ~expected_vars ~received_vars).match_
      ~ok:(fun value ->
        f value ~type_of_types ~level ~names ~expected_vars ~received_vars)
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
    let type_ = Ex_term type_ in
    fail @@ Cerror_typer_not_a_forall { type_ }

  let[@inline always] instance ~name ~type_of_types:_ ~level ~names
      ~expected_vars:_ ~received_vars:_ =
    match Name.Map.find_opt name names with
    | Some (var_level, Ex_term type_) ->
        let offset = Level.offset ~from:var_level ~to_:level in
        let type_ = Shift.shift_term ~offset type_ in
        ok @@ (offset, Ex_term type_)
    | None -> error @@ CError_typer_unknown_var { name }

  let[@inline always] with_expected_var f ~type_of_types ~level ~names
      ~expected_vars ~received_vars =
    let expected_vars = Bound :: expected_vars in
    f () ~type_of_types ~level ~names ~expected_vars ~received_vars

  let[@inline always] with_received_var ~name ~type_ f ~type_of_types ~level
      ~names ~expected_vars ~received_vars =
    let names = Name.Map.add name (level, Ex_term type_) names in
    let received_vars = Bound :: received_vars in
    (* TODO: weird, level increases first? *)
    let level = Level.next level in
    let names =
      (* TODO: why this shift here? *)
      let type_ = Shift.shift_term ~offset:Offset.one type_ in
      Name.Map.add name (level, Ex_term type_) names
    in
    let received_vars = Bound :: received_vars in
    f () ~type_of_types ~level ~names ~expected_vars ~received_vars

  let[@inline always] with_unify_context f ~type_of_types:_ ~level:_ ~names:_
      ~expected_vars ~received_vars =
    f () ~expected_vars ~received_vars

  let[@inline always] with_received_normalize_context f ~type_of_types:_
      ~level:_ ~names:_ ~expected_vars:_ ~received_vars =
    f () ~vars:received_vars

  let[@inline always] with_loc ~loc f ~type_of_types ~level ~names
      ~expected_vars ~received_vars =
    (f () ~type_of_types ~level ~names ~expected_vars ~received_vars).match_ ~ok
      ~error:(fun desc -> error @@ CError_loc { error = desc; loc })

  open Ttree

  let[@inline always] tt_type () ~type_of_types ~level ~names:_ ~expected_vars:_
      ~received_vars:_ =
    let offset = Level.offset ~from:type_of_types ~to_:level in
    ok @@ TT_var { offset }
end
