open Syntax
open Ttree
open Terror

(* TODO: benchmark with @inline and continuations *)
module Unify_context = struct
  type 'a unify_context = aliases:term Level.Map.t -> ('a, error) result
  type 'a t = 'a unify_context

  let pure value ~aliases:_ = Ok value
  let fail desc ~aliases:_ = Error desc

  let ( let* ) context f ~aliases =
    match context ~aliases with
    | Ok value -> f value ~aliases
    | Error _ as error -> error

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

  let find_free_var_alias ~var ~aliases = Ok (Level.Map.find_opt var aliases)
end

module Typer_context = struct
  type 'a typer_context =
    level:Level.t ->
    (* TODO: Hashtbl *)
    (* TODO: also think about word table as an optimization *)
    vars:(Level.t * term) Name.Map.t ->
    aliases:term Level.Map.t ->
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
    let aliases = Level.Map.empty in
    f () ~level ~vars ~aliases

  let pure value ~level:_ ~vars:_ ~aliases:_ = Ok value
  let fail desc ~level:_ ~vars:_ ~aliases:_ = Error desc

  let ( let* ) context f ~level ~vars ~aliases =
    match context ~level ~vars ~aliases with
    | Ok value -> f value ~level ~vars ~aliases
    | Error _ as error -> error

  let error_not_a_forall ~type_ = fail @@ TError_typer_not_a_forall { type_ }

  let error_pairs_not_implemented () =
    fail @@ TError_typer_pairs_not_implemented

  let error_erasable_not_implemented () =
    fail @@ TError_typer_erasable_not_implemented

  let error_unknown_extension ~extension ~payload =
    fail @@ TError_typer_unknown_extension { extension; payload }

  let error_missing_annotation () = fail @@ TError_typer_missing_annotation

  let error_unknown_native ~native =
    fail @@ TError_typer_unknown_native { native }

  let level () ~level ~vars:_ ~aliases:_ = Ok level

  let find_free_var_alias ~var ~level:_ ~vars:_ ~aliases =
    Ok (Level.Map.find_opt var aliases)

  let with_free_vars ~name ~type_ ~alias f ~level ~vars ~aliases =
    let level = Level.next level in
    let vars = Name.Map.add name (level, type_) vars in
    let aliases =
      match alias with
      (* TODO: use substitutions for this *)
      | Some alias -> Level.Map.add level alias aliases
      | None -> aliases
    in
    f () ~level ~vars ~aliases

  let lookup_var ~name ~level:_ ~vars ~aliases:_ =
    match Name.Map.find_opt name vars with
    | Some (level, type_) -> Ok (level, type_)
    | None -> Error (TError_typer_unknown_var { name })

  let free_vars ~vars =
    (* TODO: this is hackish *)
    Name.Map.fold
      (fun name (level, _type) free_vars -> Level.Map.add level name free_vars)
      vars Level.Map.empty

  let pp_term () ~level:_ ~vars ~aliases:_ =
    let bound_vars = [] in
    let free_vars = free_vars ~vars in
    Ok (Tprinter.raw_pp_term ~bound_vars ~free_vars)

  let pp_error () ~level:_ ~vars ~aliases:_ =
    let bound_vars = [] in
    let free_vars = free_vars ~vars in
    Ok (Tprinter.raw_pp_error ~bound_vars ~free_vars)

  let with_loc ~loc f ~level ~vars ~aliases =
    match f () ~level ~vars ~aliases with
    | Ok _value as ok -> ok
    | Error desc -> Error (TError_loc { error = desc; loc })

  let with_unify_context f ~level:_ ~vars:_ ~aliases = f () ~aliases
end
