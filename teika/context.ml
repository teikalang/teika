open Ttree
open Terror

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
    fail @@ TError_unify_bound_var_clash { expected; received }

  let[@inline always] error_free_var_clash ~expected ~received =
    fail @@ TError_unify_free_var_clash { expected; received }

  let[@inline always] error_type_clash ~expected ~expected_norm ~received
      ~received_norm =
    let expected = Ex_term expected in
    let received = Ex_term received in
    fail
    @@ TError_unify_type_clash
         { expected; expected_norm; received; received_norm }

  let[@inline always] error_var_occurs ~hole ~in_ =
    fail @@ TError_unify_var_occurs { hole; in_ }
end

module Typer_context = struct
  type 'a typer_context =
    level:Level.t ->
    (* TODO: Hashtbl *)
    vars:(Level.t * ex_term * ex_term option) Name.Map.t ->
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
      Name.Map.add type_name (type_level, Ex_term tt_type, None) names
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
    fail @@ TError_typer_pairs_not_implemented

  let[@inline always] error_not_a_forall ~type_ =
    let type_ = Ex_term type_ in
    fail @@ TError_typer_not_a_forall { type_ }

  let[@inline always] error_var_escape ~var =
    fail @@ TError_typer_var_escape { var }

  let[@inline always] error_typer_unknown_extension ~extension ~payload =
    fail @@ TError_typer_unknown_extension { extension; payload }

  let[@inline always] lookup_var ~name ~level:_ ~vars ~expected_vars:_
      ~received_vars:_ =
    match Name.Map.find_opt name vars with
    | Some (var, type_, alias) -> ok @@ (var, type_, alias)
    | None -> error @@ TError_typer_unknown_var { name }

  let[@inline always] with_expected_var f ~level ~vars ~expected_vars
      ~received_vars =
    let expected_vars = Free :: expected_vars in
    f () ~level ~vars ~expected_vars ~received_vars

  let[@inline always] with_received_var ~name ~type_ ~alias f ~level ~vars
      ~expected_vars ~received_vars =
    let level = Level.next level in
    let alias =
      match alias with Some alias -> Some (Ex_term alias) | None -> None
    in
    let vars = Name.Map.add name (level, Ex_term type_, alias) vars in
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
      ~error:(fun desc -> error @@ TError_loc { error = desc; loc })
end
