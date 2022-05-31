open Utils
open Language
open Type
open Env
open Unify
open Generalize

type error = Invalid_number | Not_a_type | Unimplemented

exception Error of { loc : Location.t; error : error }

let raise env error =
  let loc = current_loc env in
  raise (Error { loc; error })

(* TODO: addapt tree to new Language tree *)
type term = {
  (* exposed env *)
  t_env : Env.t;
  t_loc : Location.t;
  t_type : type_;
  t_desc : term_desc;
}

and term_desc =
  | Term_ident of Ident.t
  | Term_number of int
  (* TODO: what to put here as param? *)
  | Term_forall of { body : term }
  | Term_arrow of { param : term_pat; body : term }
  | Term_implicit_lambda of { body : term }
  | Term_explicit_lambda of { param : term_pat; body : term }
  | Term_apply of { lambda : term; arg : term }
  | Term_let of { bound : term_pat; value : term; body : term }
  | Term_struct of term_field list
  (* TODO: what to put here as content? *)
  | Term_sig
  | Term_asterisk
  | Term_annot of { value : term; type_ : term }

and term_field = {
  tf_env : Env.t;
  tf_loc : Location.t;
  tf_type : type_;
  tf_bound : term_pat;
  tf_value : term;
}

and term_pat = {
  tp_env : Env.t;
  tp_loc : Location.t;
  tp_names : (Name.t * type_) list;
  tp_type : type_;
  tp_desc : term_pat_desc;
}

and term_pat_desc =
  | Term_pat_ident of Ident.t
  | Term_pat_struct of term_pat list
  | Term_pat_annot of { pat : term_pat; type_ : term }

(* helpers *)
let extract_type env type_ =
  match desc type_ with
  | T_type { forall; type_ } -> Instance.instance_bound env ~forall type_
  | T_var (Weak _) ->
      let internal_type = new_weak_var env in
      let () =
        let expected = new_type (Forall.make ()) ~type_:internal_type in
        unify env ~expected ~received:type_
      in
      internal_type
  | _ -> raise env Not_a_type

let match_type env ~forall ~expected ~value =
  let expected = Instance.instance_weaken env ~forall expected in
  unify env ~expected ~received:value

(* term *)
let return_term env type_ desc =
  let loc = current_loc env in
  (type_, { t_env = env; t_loc = loc; t_type = type_; t_desc = desc })

let term_ident env type_ ~ident = return_term env type_ (Term_ident ident)
let term_number env type_ ~number = return_term env type_ (Term_number number)
let term_forall env type_ ~body = return_term env type_ (Term_forall { body })

let term_arrow env type_ ~param ~body =
  return_term env type_ (Term_arrow { param; body })

let term_implicit_lambda env type_ ~body =
  return_term env type_ (Term_implicit_lambda { body })

let term_explicit_lambda env type_ ~param ~body =
  return_term env type_ (Term_explicit_lambda { param; body })

let term_apply env type_ ~lambda ~arg =
  return_term env type_ (Term_apply { lambda; arg })

let term_let env type_ ~bound ~value ~body =
  return_term env type_ (Term_let { bound; value; body })

let term_field env type_ ~bound ~value =
  let loc = current_loc env in
  {
    tf_env = env;
    tf_loc = loc;
    tf_type = type_;
    tf_bound = bound;
    tf_value = value;
  }

let term_struct env type_ ~fields = return_term env type_ (Term_struct fields)
let term_sig env type_ = return_term env type_ Term_sig
let term_asterisk env type_ = return_term env type_ Term_asterisk

let term_annot env type_type ~value ~type_ =
  return_term env type_type (Term_annot { value; type_ })

(* term_pat *)
let return_pat env type_ names desc =
  let loc = current_loc env in
  ( type_,
    {
      tp_env = env;
      tp_loc = loc;
      tp_names = names;
      tp_type = type_;
      tp_desc = desc;
    },
    names,
    env )

let pat_ident env type_ name ~ident =
  let names = [ (name, type_) ] in
  return_pat env type_ names (Term_pat_ident ident)

let pat_struct env type_ names ~fields =
  return_pat env type_ names (Term_pat_struct fields)

let pat_annot env type_type names ~pat ~type_ =
  return_pat env type_type names (Term_pat_annot { pat; type_ })

let rec type_expr env term =
  (* dispatch to the proper function *)
  let { le_loc = loc; le_desc = term } = term in
  let env = set_loc loc env in

  let type_, term =
    match term with
    | LE_var name -> type_ident env ~name
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
  let { le_desc = _; le_loc = loc } = term in
  let env = set_loc loc env in

  (* TODO: this is probably wrong, make with_forall function *)
  let type_, term = type_expr env term in
  let type_ = extract_type env type_ in
  (type_, term)

and type_ident env ~name =
  let name = Name.make name in
  let ident, type_ = env |> Env.lookup name in
  term_ident env type_ ~ident

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
  let forall = Forall.make () in
  let env =
    let previous_loc = current_loc env in
    let { lp_desc = param; lp_loc = loc } = param in
    let env = set_loc loc env in
    match param with
    | LP_var name ->
        let name = Name.make name in
        (* TODO:is always small *)
        let internal_type_ = new_bound_var ~name:(Some name) forall in
        let type_ = new_type (Forall.make ()) ~type_:internal_type_ in
        (* TODO: shadowing? or duplicated name error? *)
        (* TODO: also this _ident, use it? *)
        let _ident, env = env |> Env.add name type_ in

        set_loc previous_loc env
    | _ -> raise env Unimplemented
  in
  let body_type, body = type_type env body in

  let type_ = new_forall forall ~body:body_type in
  let type_ = new_type (Forall.make ()) ~type_ in

  Forall.clear forall;
  term_forall env type_ ~body

and type_explicit_arrow env ~param ~body =
  (* TODO: param should be pattyern *)
  let forall, env = enter_forall env in
  let param_type, param, _names, env = type_pat env param in
  let body_type, body = type_type env body in
  (* TODO: what about this universe? *)
  let arrow_type_ = new_arrow ~param:param_type ~return:body_type in
  let type_ = new_forall forall ~body:arrow_type_ in
  let type_ = new_type (Forall.make ()) ~type_ in

  Forall.clear forall;
  term_arrow env type_ ~param ~body

and type_lambda env ~implicit ~param ~body =
  if implicit then type_implicit_lambda env ~param ~body
  else
    (* TODO: this should apply to implicit *)
    let lambda_type, lambda =
      let _forall, env = enter_forall env in
      type_explicit_lambda env ~param ~body
    in
    let lambda_type = generalize env lambda_type in
    (lambda_type, lambda)

and type_implicit_lambda env ~param ~body =
  let forall, env = enter_forall env in
  let env =
    let previous_loc = current_loc env in
    let { lp_desc = param; lp_loc = loc } = param in
    let env = set_loc loc env in

    match param with
    | LP_var name ->
        let name = Name.make name in
        (* TODO: name for variables *)
        let internal_type = new_bound_var ~name:(Some name) forall in
        let type_ = new_type (Forall.make ()) ~type_:internal_type in
        (* TODO: shadowing? or duplicated name error? *)
        (* TODO: also this _ident, use it? *)
        let _ident, env = env |> Env.add name type_ in

        let env = set_loc previous_loc env in
        env
    | _ -> raise env Unimplemented
  in
  let body_type, body = type_expr env body in
  let type_ = new_forall forall ~body:body_type in

  Forall.clear forall;
  term_implicit_lambda env type_ ~body

and type_explicit_lambda env ~param ~body =
  let forall, env = enter_forall env in

  let param_type, param, _names, env = type_pat env param in
  let body_type, body = type_expr env body in

  let arrow_type_ = new_arrow ~param:param_type ~return:body_type in
  let type_ = new_forall forall ~body:arrow_type_ in

  Forall.clear forall;
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

  let env = set_loc previous_loc env in
  (forall, bound_type, bound, value, names, env)

and type_let env ~bind ~body =
  let forall, _bound_type, bound, value, _names, env = type_bind env ~bind in
  let body_type, body = type_expr env body in
  (* TODO: why clear here? *)
  Forall.clear forall;
  term_let env body_type ~bound ~value ~body

and type_record env ~fields =
  let fields, names, env =
    List.fold_left
      (fun (body_fields, body_names, env) bind ->
        (* TODO: what about this forall? *)
        let _forall, bound_type, bound, value, names, env =
          type_bind env ~bind
        in

        let field = term_field env bound_type ~bound ~value in

        let names = names @ body_names in
        let fields = field :: body_fields in
        (fields, names, env))
      ([], [], env) fields
  in
  let names = List.rev names in
  let fields = List.rev fields in

  let fields_type = List.map (fun (name, type_) -> { name; type_ }) names in
  let type_ = new_struct ~fields:fields_type in
  term_struct env type_ ~fields

and type_signature env ~fields =
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
  let type_ = new_type (Forall.make ()) ~type_ in
  term_sig env type_

and type_asterisk env =
  let forall, env = enter_forall env in
  let type_ = new_bound_var ~name:None forall in
  (* TODO: this is VERY weird, nested T_type *)
  let type_ = new_type forall ~type_ in
  let type_ = new_type forall ~type_ in

  Forall.clear forall;
  term_asterisk env type_

and type_annot env ~value ~type_ =
  (* TODO: fail when inside of type_ *)
  let forall, env = enter_forall env in

  let value_type, value = type_expr env value in
  let type_type, type_ = type_type env type_ in
  match_type env ~forall ~expected:type_type ~value:value_type;

  Forall.clear forall;
  term_annot env type_type ~value ~type_

and type_pat env term =
  let previous_loc = current_loc env in
  let { lp_loc = loc; lp_desc = term } = term in
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
  let ident, env = Env.add name type_ env in
  pat_ident env type_ name ~ident

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
  pat_struct env type_ names ~fields

and type_pat_annot env ~pat ~type_ =
  let pat_type, pat, names, env = type_pat env pat in
  let type_type, type_ = type_type env type_ in

  let () = unify env ~expected:type_type ~received:pat_type in
  pat_annot env type_type names ~pat ~type_
