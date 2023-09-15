open Ttree
open Terror

(* TODO: benchmark with @inline and continuations *)
module Var_context = struct
  type 'a var_context = level:Level.t -> ('a, error) result
  type 'a t = 'a var_context

  let pure value ~level:_ = Ok value
  let fail desc ~level:_ = Error desc

  let ( let* ) context f ~level =
    match context ~level with
    | Ok value -> f value ~level
    | Error _ as error -> error

  let error_subst_found term = fail @@ TError_misc_subst_found { term }
  let error_bound_var_found term = fail @@ TError_misc_bound_var_found { term }
  let error_unfold_found term = fail @@ TError_misc_unfold_found { term }
  let error_annot_found term = fail @@ TError_misc_annot_found { term }
  let error_var_occurs ~hole ~in_ = fail @@ TError_misc_var_occurs { hole; in_ }
  let error_var_escape ~var = fail @@ TError_misc_var_escape { var }

  let with_free_var f ~level =
    let level = Level.next level in
    f () ~level

  let level () ~level = Ok level
end

module Unify_context = struct
  type 'a unify_context = level:Level.t -> ('a, error) result
  type 'a t = 'a unify_context

  let pure value ~level:_ = Ok value
  let fail desc ~level:_ = Error desc

  let ( let* ) context f ~level =
    match context ~level with
    | Ok value -> f value ~level
    | Error _ as error -> error

  let error_subst_found ~expected ~received =
    fail @@ TError_unify_subst_found { expected; received }

  let error_bound_var_found ~expected ~received =
    fail @@ TError_unify_bound_var_found { expected; received }

  let error_unfold_found ~expected ~received =
    fail @@ TError_unify_unfold_found { expected; received }

  let error_annot_found ~expected ~received =
    fail @@ TError_unify_annot_found { expected; received }

  let error_bound_var_clash ~expected ~received =
    fail @@ TError_unify_bound_var_clash { expected; received }

  let error_free_var_clash ~expected ~received =
    fail @@ TError_unify_free_var_clash { expected; received }

  let error_type_clash ~expected ~received =
    fail @@ TError_unify_type_clash { expected; received }

  let error_string_clash ~expected ~received =
    fail @@ TError_unify_string_clash { expected; received }

  let with_free_vars f ~level =
    let level = Level.next level in
    f () ~level

  (* context *)
  let with_expected_var_context f ~level = f () ~level
  let with_received_var_context f ~level = f () ~level
end

module Typer_context = struct
  type 'a typer_context =
    level:Level.t ->
    (* TODO: Hashtbl *)
    (* TODO: also think about word table as an optimization *)
    vars:(Level.t * term) Name.Map.t ->
    subst:subst ->
    ('a, error) result

  type 'a t = 'a typer_context

  let run f =
    let level = string_level in
    let names = Name.Map.empty in
    let vars =
      (* TODO: better place for constants *)
      names
      |> Name.Map.add (Name.make "Type") (type_level, tt_type)
      |> Name.Map.add (Name.make "String") (string_level, tt_type)
    in
    let subst =
      let type_ = TType { desc = TT_free_var { level = type_level } } in
      let string = TType { desc = TT_free_var { level = string_level } } in
      let subst = TS_open { to_ = type_ } in
      TS_cons { subst = TS_open { to_ = string }; next = TS_lift { subst } }
    in
    (* TODO: should Type be here? *)
    f () ~level ~vars ~subst

  let pure value ~level:_ ~vars:_ ~subst:_ = Ok value
  let fail desc ~level:_ ~vars:_ ~subst:_ = Error desc

  let ( let* ) context f ~level ~vars ~subst =
    match context ~level ~vars ~subst with
    | Ok value -> f value ~level ~vars ~subst
    | Error _ as error -> error

  let error_pairs_not_implemented () =
    fail @@ TError_typer_pairs_not_implemented

  let error_unknown_extension ~extension ~payload =
    fail @@ TError_typer_unknown_extension { extension; payload }

  let error_unknown_native ~native =
    fail @@ TError_typer_unknown_native { native }

  let level () ~level ~vars:_ ~subst:_ = Ok level

  let enter_level f ~level ~vars ~subst =
    let level = Level.next level in
    f () ~level ~vars ~subst

  let with_free_vars ~name ~type_ ~alias f ~level ~vars ~subst =
    (* make alias and type_, context independent *)
    let alias =
      match alias with
      | Some alias ->
          (* TODO: maybe check alias type? *)
          Some (TTerm { desc = TT_subst { term = alias; subst }; type_ })
      | None -> None
    in
    let type_ = TType { desc = TT_subst { term = type_; subst } } in
    let level = Level.next level in
    let vars = Name.Map.add name (level, type_) vars in
    let subst =
      let to_ =
        match alias with
        | Some alias -> alias
        | None -> TTerm { desc = TT_free_var { level }; type_ }
      in
      TS_cons { subst = TS_open { to_ }; next = TS_lift { subst } }
    in
    f () ~level ~vars ~subst

  let lookup_var ~name ~level:current ~vars ~subst:_ =
    match Name.Map.find_opt name vars with
    | Some (from, type_) ->
        let index = Level.offset ~from ~to_:current in
        Ok (index, type_)
    | None -> Error (TError_typer_unknown_var { name })

  let with_loc ~loc f ~level ~vars ~subst =
    match f () ~level ~vars ~subst with
    | Ok _value as ok -> ok
    | Error desc -> Error (TError_loc { error = desc; loc })

  let with_var_context f ~level ~vars:_ ~subst = f subst ~level
  let with_unify_context f ~level ~vars:_ ~subst = f subst ~level
end
