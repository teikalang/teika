open Utils
open Language
open Type
open Env
open Unify
open Tree

type error = Invalid_number | Not_a_type | Unimplemented

exception Error of { loc : Location.t; error : error }

let raise env error =
  let loc = current_loc env in
  raise (Error { loc; error })

(* helpers *)
let wrap_type type_ = new_type (Forall.generic ()) ~type_

let extract_type env type_ =
  match desc type_ with
  | T_type { forall; type_ } -> Instance.instance_bound env ~forall type_
  | T_var (Weak _) ->
      let internal_type = new_weak_var env in
      let () =
        let expected = wrap_type internal_type in
        unify env ~expected ~received:type_
      in
      internal_type
  | _ -> raise env Not_a_type

let bind_forall forall ~body =
  Forall.universal forall;
  new_forall forall ~body

let bind_type forall ~type_ =
  Forall.existential forall;
  new_type forall ~type_

let match_type env ~forall ~expected ~value =
  let expected = Instance.instance_weaken env ~forall expected in
  unify env ~expected ~received:value

(* term *)
let return_term env type_ desc =
  let loc = current_loc env in
  (type_, TE { env; loc; type_; desc })

let term_var env type_ ~var = return_term env type_ (TE_var var)
let term_number env type_ ~number = return_term env type_ (TE_number number)
let term_forall env type_ ~body = return_term env type_ (TE_forall { body })

let term_arrow env type_ ~param ~body =
  return_term env type_ (TE_arrow { param; body })

let term_implicit_lambda env type_ ~body =
  return_term env type_ (TE_implicit_lambda { body })

let term_explicit_lambda env type_ ~param ~body =
  return_term env type_ (TE_explicit_lambda { param; body })

let term_apply env type_ ~lambda ~arg =
  return_term env type_ (TE_apply { lambda; arg })

let term_let env type_ ~bind ~body =
  return_term env type_ (TE_let { bind; body })

let term_bind env type_ ~names ~bound ~value =
  let loc = current_loc env in
  TE_bind { env; loc; names; type_; bound; value }

let term_record env type_ ~fields = return_term env type_ (TE_record fields)
let term_signature env type_ = return_term env type_ TE_signature
let term_asterisk env type_ = return_term env type_ TE_asterisk

let term_annot env type_type ~value ~type_ =
  return_term env type_type (TE_annot { value; type_ })

(* term_pat *)
let return_pat env type_ names desc =
  let loc = current_loc env in
  (type_, TP { env; loc; names; type_; desc }, names, env)

let pat_var env type_ name ~var =
  let names = [ (name, type_) ] in
  return_pat env type_ names (TP_var var)

let pat_record env type_ names ~fields =
  return_pat env type_ names (TP_record fields)

let pat_annot env type_type names ~pat ~type_ =
  return_pat env type_type names (TP_annot { pat; type_ })

let rec type_expr env term =
  (* dispatch to the proper function *)
  let (LE { loc; desc = term }) = term in
  let env = set_loc loc env in

  let type_, term =
    match term with
    | LE_var name -> type_var env ~name
    | LE_number number -> type_number env ~number
    | LE_arrow { implicit; param; body } ->
        type_arrow env ~implicit ~param ~body
    | LE_lambda { implicit; param; body } ->
        type_lambda env ~implicit ~param ~body
    | LE_apply { lambda; arg } -> type_apply env ~lambda ~arg
    | LE_let { bind; body } -> type_let env ~bind ~body
    | LE_record fields -> type_record env ~fields
    | LE_signature fields -> type_signature env ~fields
    | LE_asterisk -> type_asterisk env
    | LE_annot { value; type_ } -> type_annot env ~value ~type_
  in
  (type_, term)

and type_type env term =
  (* TODO: this function is not great*)
  let (LE { desc = _; loc }) = term in
  let env = set_loc loc env in

  (* TODO: this is probably wrong, make with_forall function *)
  let type_, term = type_expr env term in
  let type_ = extract_type env type_ in
  (type_, term)

and type_var env ~name =
  let name = Name.make name in
  let var, type_ = env |> Env.lookup name in
  term_var env type_ ~var

and type_number env ~number =
  let number =
    match int_of_string_opt number with
    | Some number -> number
    | None -> raise env Invalid_number
  in

  (* TODO: what about thehere? *)
  let type_ = Env.int_type in
  term_number env type_ ~number

and type_arrow env ~implicit ~param ~body =
  if implicit then type_implicit_arrow env ~param ~body
  else type_explicit_arrow env ~param ~body

and type_implicit_arrow env ~param ~body =
  let forall, env = enter_forall env in
  let env =
    let previous_loc = current_loc env in
    let (LP { desc = param; loc }) = param in
    let env = set_loc loc env in
    match param with
    | LP_var name ->
        let name = Name.make name in
        (* TODO:is always small *)
        let internal_type_ = new_bound_var ~name:(Some name) forall in
        let type_ = wrap_type internal_type_ in
        (* TODO: shadowing? or duplicated name error? *)
        (* TODO: also this _ident, use it? *)
        let _ident, env = env |> Env.add name type_ in

        set_loc previous_loc env
    | _ -> raise env Unimplemented
  in
  let body_type, body = type_type env body in

  let type_ = bind_forall forall ~body:body_type in
  let type_ = wrap_type type_ in

  term_forall env type_ ~body

and type_explicit_arrow env ~param ~body =
  (* TODO: param should be pattyern *)
  let forall, env = enter_forall env in
  let param_type, param, _names, env = type_pat env param in
  let body_type, body = type_type env body in
  (* TODO: what about this universe? *)
  let arrow_type_ = new_arrow ~param:param_type ~return:body_type in
  let type_ = bind_forall forall ~body:arrow_type_ in
  let type_ = wrap_type type_ in

  term_arrow env type_ ~param ~body

and type_lambda env ~implicit ~param ~body =
  if implicit then type_implicit_lambda env ~param ~body
  else type_explicit_lambda env ~param ~body

and type_implicit_lambda env ~param ~body =
  let forall, env = enter_forall env in
  let env =
    let previous_loc = current_loc env in
    let (LP { desc = param; loc }) = param in
    let env = set_loc loc env in

    match param with
    | LP_var name ->
        let name = Name.make name in
        (* TODO: name for variables *)
        let internal_type = new_bound_var ~name:(Some name) forall in
        let type_ = wrap_type internal_type in
        (* TODO: shadowing? or duplicated name error? *)
        (* TODO: also this _ident, use it? *)
        let _ident, env = env |> Env.add name type_ in

        let env = set_loc previous_loc env in
        env
    | _ -> raise env Unimplemented
  in
  let body_type, body = type_expr env body in
  let type_ = bind_forall forall ~body:body_type in

  term_implicit_lambda env type_ ~body

and type_explicit_lambda env ~param ~body =
  let forall, env = enter_forall env in

  let param_type, param, _names, env = type_pat env param in
  let body_type, body = type_expr env body in

  let arrow_type_ = new_arrow ~param:param_type ~return:body_type in
  let type_ = bind_forall forall ~body:arrow_type_ in

  term_explicit_lambda env type_ ~param ~body

and type_apply env ~lambda ~arg =
  (* TODO: applying a type is different from applying a term? *)
  let lambda_type, lambda = type_expr env lambda in
  let arg_type, arg = type_expr env arg in

  let return_type = new_weak_var env in
  let () =
    let expected = new_arrow ~param:arg_type ~return:return_type in
    unify env ~expected ~received:lambda_type
  in

  term_apply env return_type ~lambda ~arg

and type_bind env ~bind =
  let previous_loc = current_loc env in
  let (LE_bind { loc; bound; value }) = bind in
  let env = set_loc loc env in

  let forall, env = enter_forall env in

  let value_type, value = type_expr env value in
  let bound_type, bound, names, env = type_pat env bound in
  match_type env ~forall ~expected:bound_type ~value:value_type;

  let bind = term_bind env bound_type ~names ~bound ~value in
  let env = set_loc previous_loc env in
  (forall, bind, names, env)

and type_let env ~bind ~body =
  let forall, bind, _names, inner_env = type_bind env ~bind in
  let body_type, body = type_expr inner_env body in

  let let_type_ = Instance.instance_bound env ~forall body_type in
  term_let env let_type_ ~bind ~body

and type_record env ~fields =
  let fields, names, env =
    List.fold_left
      (fun (body_binds, body_names, env) bind ->
        let _forall, bind, names, env = type_bind env ~bind in
        let names = names @ body_names in
        let fields = bind :: body_binds in
        (fields, names, env))
      ([], [], env) fields
  in
  let names = List.rev names in
  let fields = List.rev fields in

  let fields_type = List.map (fun (name, type_) -> { name; type_ }) names in
  let type_ = new_struct ~fields:fields_type in
  term_record env type_ ~fields

and type_signature env ~fields =
  (* TODO: each field should have it's own forall*)
  let forall, env = enter_forall env in

  let names, env =
    List.fold_left
      (fun (body_names, env) bound ->
        let _type, _field, field_names, env = type_pat env bound in
        let names = field_names @ body_names in
        (names, env))
      ([], env) fields
  in
  let names = List.rev names in
  let fields = List.map (fun (name, type_) -> { name; type_ }) names in
  let type_ = new_struct ~fields in
  let type_ = bind_type forall ~type_ in

  term_signature env type_

and type_asterisk env =
  let forall, env = enter_forall env in
  let type_ = new_bound_var ~name:None forall in
  (* TODO: this is VERY weird, nested T_type *)
  let type_ = wrap_type type_ in
  let type_ = bind_type forall ~type_ in

  term_asterisk env type_

and type_annot env ~value ~type_ =
  (* TODO: fail when inside of type_ *)
  let forall, inner_env = enter_forall env in

  let value_type, value = type_expr inner_env value in
  let type_type, type_ = type_type inner_env type_ in
  match_type inner_env ~forall ~expected:type_type ~value:value_type;

  let annot_type_ = Instance.instance_bound env ~forall type_type in
  term_annot env annot_type_ ~value ~type_

and type_pat env term =
  let previous_loc = current_loc env in
  let (LP { loc; desc = term }) = term in
  let env = set_loc loc env in

  let type_, term, names, env =
    match term with
    | LP_var name -> type_pat_ident env ~name
    | LP_record fields -> type_pat_record env ~fields
    | LP_annot { pat; type_ } -> type_pat_annot env ~pat ~type_
  in
  let env = set_loc previous_loc env in
  (type_, term, names, env)

and type_pat_ident env ~name =
  (* TODO: what happens if a pattern introduces the same name twice? *)
  let name = Name.make name in
  let type_ = new_weak_var env in
  let var, env = Env.add name type_ env in
  pat_var env type_ name ~var

and type_pat_record env ~fields =
  let fields, names, env =
    List.fold_left
      (fun (body_fields, body_names, env) bound ->
        let _type, field, names, env = type_pat env bound in

        let fields = field :: body_fields in
        (* TODO: unique name *)
        let names = names @ body_names in

        (fields, names, env))
      ([], [], env) fields
  in
  let names = List.rev names in
  let fields = List.rev fields in
  let fields_type = List.map (fun (name, type_) -> { name; type_ }) names in
  let type_ = new_struct ~fields:fields_type in
  pat_record env type_ names ~fields

and type_pat_annot env ~pat ~type_ =
  let pat_type, pat, names, env = type_pat env pat in
  let type_type, type_ = type_type env type_ in

  let () = unify env ~expected:type_type ~received:pat_type in
  pat_annot env type_type names ~pat ~type_
