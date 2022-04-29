open Utils
open Syntax
open Type
open Env
open Unify
open Generalize

type error =
  | Binding_without_value
  | Binding_without_body
  | Invalid_number
  | Not_a_type
  | Unimplemented

exception Error of { loc : Location.t; error : error }

let raise loc error = raise (Error { loc; error })

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
  | Term_arrow of { param : term; body : term }
  | Term_implicit_lambda of { body : term }
  | Term_explicit_lambda of { param : term_pat; body : term }
  | Term_apply of { lambda : term; arg : term }
  | Term_let of { bound : term_pat; value : term; body : term }
  | Term_struct of term_field list
  (* TODO: what to put here as content? *)
  | Term_sig
  | Term_annot of { value : term; type_ : term }

and term_field = {
  tf_env : Env.t;
  tf_loc : Location.t;
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
  | Term_pat_annot of { pat : term_pat; type_ : term }

(* helpers *)
let dot_name = Name.make "."
let new_dot_struct type_ = new_struct ~fields:[ { name = dot_name; type_ } ]

let extract_type env loc type_ =
  let open Utils in
  match desc type_ with
  | T_forall _ -> type_
  (* TODO: problem, a lambda cannot be returned from a type, or can it?

     x: A => A; What does this mean? *)
  | T_arrow _ -> raise loc Unimplemented
  | T_struct fields -> (
      (* TODO: forgive my for this *)
      let dot_type =
        List.find_map
          (fun { name; type_ } ->
            if Name.equal name dot_name then Some type_ else None)
          fields
      in
      match dot_type with Some type_ -> type_ | None -> raise loc Not_a_type)
  | T_var _ ->
      (* TODO: this can be linked directly if weaken *)
      let dot_type = new_weak_var env in
      let expected = new_dot_struct dot_type in
      let () = unify ~loc env ~expected ~received:type_ in
      dot_type

(* term *)
let return_term env loc type_ desc =
  (type_, { t_env = env; t_loc = loc; t_type = type_; t_desc = desc }, env)

let term_ident env loc type_ ~ident =
  return_term env loc type_ (Term_ident ident)

let term_number env loc type_ ~number =
  return_term env loc type_ (Term_number number)

let term_forall env loc dot_type ~body =
  let type_ = new_dot_struct dot_type in
  return_term env loc type_ (Term_forall { body })

let term_arrow env loc dot_type ~param ~body =
  let type_ = new_dot_struct dot_type in
  return_term env loc type_ (Term_arrow { param; body })

let term_implicit_lambda env loc type_ ~body =
  return_term env loc type_ (Term_implicit_lambda { body })

let term_explicit_lambda env loc type_ ~param ~body =
  return_term env loc type_ (Term_explicit_lambda { param; body })

let term_lambda env loc type_ ~lambda ~arg =
  return_term env loc type_ (Term_apply { lambda; arg })

let term_let env loc type_ ~bound ~value ~body =
  return_term env loc type_ (Term_let { bound; value; body })

let term_field env loc type_ ~bound ~value =
  let field =
    { tf_env = env; tf_loc = loc; tf_bound = bound; tf_value = value }
  in
  (type_, field, env)

let term_struct env loc type_ ~fields =
  return_term env loc type_ (Term_struct fields)

let term_sig env loc type_ = return_term env loc type_ Term_sig

let term_annot env loc type_type ~value ~type_ =
  return_term env loc type_type (Term_annot { value; type_ })

(* term_pat *)
let return_pat env loc type_ names desc =
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

let pat_ident env loc type_ names ~ident =
  return_pat env loc type_ names (Term_pat_ident ident)

let pat_annot env loc type_type names ~pat ~type_ =
  return_pat env loc type_type names (Term_pat_annot { pat; type_ })

let rec type_term env term =
  (* dispatch to the proper function *)
  let { s_loc = loc; s_desc = term } = term in
  match term with
  | S_ident name -> type_ident env loc ~name
  | S_number number -> type_number env loc ~number
  | S_arrow { param; body } -> type_arrow env loc ~param ~body
  | S_lambda { param; body } -> type_lambda env loc ~param ~body
  | S_apply { lambda; arg } -> type_apply env loc ~lambda ~arg
  | S_bind { bound; value; body } -> type_bind env loc ~bound ~value ~body
  | S_struct content -> type_struct env loc ~content
  | S_annot { value; type_ } -> type_annot env loc ~value ~type_
  | _ -> raise loc Unimplemented

and type_type env term =
  (* TODO: this function is not great*)
  let { s_desc = _; s_loc = loc } = term in
  let type_, term, env = type_term env term in
  let type_ = extract_type env loc type_ in
  (type_, term, env)

and type_ident env loc ~name =
  let name = Name.make name in
  let ident, type_ = env |> Env.lookup loc name in
  term_ident env loc type_ ~ident

and type_number env loc ~number =
  let number =
    match int_of_string_opt number with
    | Some number -> number
    | None -> raise loc Invalid_number
  in

  let type_ = Env.int_type in
  term_number env loc type_ ~number

and type_arrow env loc ~param ~body =
  match param.s_desc with
  (* TODO: this is a hack, needs proper way to distinguish between pattern
     on structure and implicit *)
  | S_struct (Some ({ s_desc = S_ident _; s_loc = _ } as param)) ->
      type_implicit_arrow env loc ~param ~body
  | _ -> type_explicit_arrow env loc ~param ~body

and type_implicit_arrow env loc ~param ~body =
  let forall = Forall_id.next () in
  let env =
    let { s_desc = param; s_loc = loc } = param in
    match param with
    | S_ident name ->
        let name = Name.make name in
        (* TODO: name for variables *)
        let dot_type = new_bound_var ~name:None forall in
        let type_ = new_dot_struct dot_type in
        (* TODO: shadowing? or duplicated name error? *)
        (* TODO: also this _ident, use it? *)
        let _ident, env = env |> Env.add loc name type_ in
        env
    | _ -> raise loc Unimplemented
  in
  let body_type, body, env = type_type env body in
  let type_ = new_forall forall ~body:body_type in
  term_forall env loc type_ ~body

and type_explicit_arrow env loc ~param ~body =
  (* TODO: param should be pattyern *)
  let param_type, param, env = type_type env param in
  let body_type, body, env = type_type env body in
  let type_ = new_arrow ~param:param_type ~return:body_type in
  term_arrow env loc type_ ~param ~body

and type_lambda env loc ~param ~body =
  match param.s_desc with
  (* TODO: what if it's None? *)
  | S_struct (Some param) -> type_implicit_lambda env ~loc ~param ~body
  | _ ->
      (* TODO: this should apply to implicit *)
      let lambda_type, lambda, _env =
        let env = enter_rank env in
        type_explicit_lambda env ~loc ~param ~body
      in
      let lambda_type = generalize env lambda_type in
      (lambda_type, lambda, env)

and type_implicit_lambda env ~loc ~param ~body =
  let forall = Forall_id.next () in
  let env = Env.enter_forall ~forall env in
  let env =
    let { s_desc = param; s_loc = loc } = param in
    match param with
    | S_ident name ->
        let name = Name.make name in
        (* TODO: name for variables *)
        let dot_type = new_bound_var ~name:None forall in
        let type_ = new_dot_struct dot_type in
        (* TODO: shadowing? or duplicated name error? *)
        (* TODO: also this _ident, use it? *)
        let _ident, env = env |> Env.add loc name type_ in
        env
    | _ -> raise loc Unimplemented
  in
  let body_type, body, _env = type_term env body in
  let type_ = new_forall forall ~body:body_type in
  term_implicit_lambda env loc type_ ~body

and type_explicit_lambda env ~loc ~param ~body =
  let param_type, param, _names, env = type_pat env param in
  let body_type, body, _env = type_term env body in
  let type_ = new_arrow ~param:param_type ~return:body_type in
  term_explicit_lambda env loc type_ ~param ~body

and type_apply env loc ~lambda ~arg =
  (* TODO: applying a type is different from applying a term? *)
  let lambda_type, lambda, _env = type_term env lambda in
  let arg_type, arg, _env = type_term env arg in

  let return_type = new_weak_var env in
  let () =
    let expected = new_arrow ~param:arg_type ~return:return_type in
    unify ~loc env ~expected ~received:lambda_type
  in

  term_lambda env loc return_type ~lambda ~arg

and type_bind env loc ~bound ~value ~body =
  let value =
    match value with
    | Some value -> value
    | None -> raise loc Binding_without_value
  in
  let body =
    match body with Some body -> body | None -> raise loc Binding_without_body
  in
  (* a bind with body and value can be described as a le
     t *)
  type_let env loc ~bound ~value ~body

and type_let env loc ~bound ~value ~body =
  (* typing value first to prevent recursion *)
  let value_type, value, _env = type_term env value in

  (* body *)
  let bound_type, bound, _names, inner_env = type_pat env bound in
  let () = unify ~loc env ~expected:bound_type ~received:value_type in
  let body_type, body, _env = type_term inner_env body in

  term_let env loc body_type ~bound ~value ~body

and type_struct env loc ~content =
  match content with
  | Some content -> (* TODO: type_struct *) type_sig env loc ~content
  | None ->
      (* TODO: does this make sense? *)
      let dot_type = new_struct ~fields:[] in
      let type_ = new_dot_struct dot_type in
      term_struct env loc type_ ~fields:[]

and type_sig env loc ~content =
  let fields_type, env = type_sig_content env ~content in
  let type_ =
    let dot_type = new_struct ~fields:fields_type in
    new_dot_struct dot_type
  in
  term_sig env loc type_

and type_sig_content env ~content =
  let { s_desc = content; s_loc = loc } = content in
  match content with
  | S_bind { bound; value; body } ->
      (match value with Some _ -> raise loc Unimplemented | None -> ());
      let field_type, env = type_sig_field env ~bound in
      let fields_type, env =
        match body with
        | Some body -> type_sig_content env ~content:body
        | None -> ([], env)
      in

      let fields_type = field_type :: fields_type in
      (fields_type, env)
  | _ -> raise loc Unimplemented

and type_sig_field env ~bound =
  let { s_desc = bound; s_loc = loc } = bound in
  match bound with
  | S_annot { value; type_ } ->
      let name =
        let { s_desc = value; s_loc = loc } = value in
        match value with
        | S_ident name -> Name.make name
        | _ -> raise loc Unimplemented
      in
      (* TODO: use this *)
      let type_type, _type_, env = type_type env type_ in
      let field_type = { name; type_ = type_type } in
      (field_type, env)
  | _ -> raise loc Unimplemented

and type_annot env loc ~value ~type_ =
  (* TODO: fail when inside of type_ *)
  let value_type, value, _env = type_term env value in
  let type_type, type_, _env = type_type env type_ in
  let () = unify ~loc env ~expected:type_type ~received:value_type in

  term_annot env loc type_type ~value ~type_

and type_pat env term =
  let { s_desc = term; s_loc = loc } = term in
  match term with
  | S_ident name -> type_pat_ident env loc ~name
  | S_annot { value = pat; type_ } -> type_pat_annot env loc ~pat ~type_
  | _ -> raise loc Unimplemented

and type_pat_ident env loc ~name =
  (* TODO: what happens if a pattern introduces the same name twice? *)
  let name = Name.make name in
  let type_ = new_weak_var env in

  let names = [ (name, type_) ] in
  let ident, env = Env.add loc name type_ env in
  pat_ident env loc type_ names ~ident

and type_pat_annot env loc ~pat ~type_ =
  let pat_type, pat, names, env = type_pat env pat in
  let type_type, type_, env = type_type env type_ in

  let () = unify ~loc env ~expected:pat_type ~received:type_type in
  pat_annot env loc type_type names ~pat ~type_
