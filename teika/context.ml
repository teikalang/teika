open Ttree

type error =
  (* TODO: why track nested locations?
       Probably because things like macros exists *)
  | CError_loc of { error : error; loc : Location.t [@opaque] }
  (* unify *)
  | CError_unify_bound_var_clash of { expected : Index.t; received : Index.t }
  | CError_unify_free_var_clash of { expected : Level.t; received : Level.t }
  | CError_unify_type_clash of {
      expected : ex_term; [@printer Tprinter.pp_ex_term]
      expected_norm : core term; [@printer Tprinter.pp_term]
      received : ex_term; [@printer Tprinter.pp_ex_term]
      received_norm : core term; [@printer Tprinter.pp_term]
    }
    (* TODO: lazy names for errors *)
  | CError_unify_var_occurs of {
      hole : ex_term hole; [@printer Tprinter.pp_ex_term_hole]
      in_ : ex_term hole; [@printer Tprinter.pp_ex_term_hole]
    }
  (* typer *)
  | CError_typer_unknown_var of { name : Name.t }
  | CError_typer_not_a_forall of {
      type_ : ex_term; [@printer Tprinter.pp_ex_term]
    }
  | CError_typer_pairs_not_implemented
  | CError_typer_var_escape of { var : Level.t }
  | CError_typer_unknown_extension of {
      extension : Name.t;
      payload : Ltree.term;
    }
[@@deriving show { with_path = false }]

type ('a, 'b) result = { match_ : 'k. ok:('a -> 'k) -> error:('b -> 'k) -> 'k }
[@@ocaml.unboxed]

let[@inline always] ok value = { match_ = (fun ~ok ~error:_ -> ok value) }
let[@inline always] error desc = { match_ = (fun ~ok:_ ~error -> error desc) }

type var_info = Free

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

  let[@inline always] error_bound_var_clash ~expected ~received =
    fail @@ CError_unify_bound_var_clash { expected; received }

  let[@inline always] error_free_var_clash ~expected ~received =
    fail @@ CError_unify_free_var_clash { expected; received }

  let[@inline always] error_type_clash ~expected ~expected_norm ~received
      ~received_norm =
    let expected = Ex_term expected in
    let received = Ex_term received in
    fail
    @@ CError_unify_type_clash
         { expected; expected_norm; received; received_norm }

  let[@inline always] error_var_occurs ~hole ~in_ =
    fail @@ CError_unify_var_occurs { hole; in_ }
end

module Typer_context = struct
  type 'a typer_context =
    level:Level.t ->
    (* TODO: Hashtbl *)
    vars:(Level.t * ex_term) Name.Map.t ->
    expected_vars:var_info list ->
    received_vars:var_info list ->
    ('a, error) result

  type 'a t = 'a typer_context

  let[@inline always] run f =
    let level = type_level in
    let names = Name.Map.empty in
    let type_name = Name.make "Type" in
    let vars =
      (* TODO: better place for constants *)
      Name.Map.add type_name (type_level, Ex_term tt_type) names
    in
    (* TODO: use proper stack *)
    let received_vars = [ Free ] in
    let expected_vars = received_vars in
    (* TODO: should Type be here? *)
    (f () ~level ~vars ~expected_vars ~received_vars).match_
      ~ok:(fun value -> Ok value)
      ~error:(fun desc -> Error desc)

  let[@inline always] test ~level ~vars ~expected_vars ~received_vars f =
    (f () ~level ~vars ~expected_vars ~received_vars).match_
      ~ok:(fun value -> Ok value)
      ~error:(fun desc -> Error desc)

  let[@inline always] return_raw value ~level:_ ~vars:_ ~expected_vars:_
      ~received_vars:_ =
    value

  let[@inline always] return value = return_raw @@ ok value
  let[@inline always] fail desc = return_raw @@ error desc

  let[@inline always] bind context f ~level ~vars ~expected_vars ~received_vars
      =
    (context ~level ~vars ~expected_vars ~received_vars).match_
      ~ok:(fun value -> f value ~level ~vars ~expected_vars ~received_vars)
      ~error

  let ( let* ) = bind

  let[@inline always] ( let+ ) context f =
    let* value = context in
    return @@ f value

  let[@inline always] error_pairs_not_implemented () =
    fail @@ CError_typer_pairs_not_implemented

  let[@inline always] error_not_a_forall ~type_ =
    let type_ = Ex_term type_ in
    fail @@ CError_typer_not_a_forall { type_ }

  let[@inline always] error_var_escape ~var =
    fail @@ CError_typer_var_escape { var }

  let[@inline always] error_typer_unknown_extension ~extension ~payload =
    fail @@ CError_typer_unknown_extension { extension; payload }

  let[@inline always] lookup_var ~name ~level:_ ~vars ~expected_vars:_
      ~received_vars:_ =
    match Name.Map.find_opt name vars with
    | Some (var, type_) -> ok @@ (var, type_)
    | None -> error @@ CError_typer_unknown_var { name }

  let[@inline always] with_expected_var f ~level ~vars ~expected_vars
      ~received_vars =
    let expected_vars = Free :: expected_vars in
    f () ~level ~vars ~expected_vars ~received_vars

  let[@inline always] with_received_var ~name ~type_ f ~level ~vars
      ~expected_vars ~received_vars =
    let level = Level.next level in
    let vars = Name.Map.add name (level, Ex_term type_) vars in
    let received_vars = Free :: received_vars in
    f () ~level ~vars ~expected_vars ~received_vars

  let[@inline always] level () ~level ~vars:_ ~expected_vars:_ ~received_vars:_
      =
    ok level

  let[@inline always] enter_level f ~level ~vars ~expected_vars ~received_vars =
    let level = Level.next level in
    f () ~level ~vars ~expected_vars ~received_vars

  let[@inline always] with_unify_context f ~level:_ ~vars:_ ~expected_vars
      ~received_vars =
    f () ~expected_vars ~received_vars

  let[@inline always] with_loc ~loc f ~level ~vars ~expected_vars ~received_vars
      =
    (f () ~level ~vars ~expected_vars ~received_vars).match_ ~ok
      ~error:(fun desc -> error @@ CError_loc { error = desc; loc })
end
