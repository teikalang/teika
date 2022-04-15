open Syntax
open Type
open Tree
open Unify
open Generalize

type error =
  | Binding_without_value
  | Binding_without_body
  | Invalid_number
  | Unimplemented

exception Error of { loc : Location.t; error : error }

let raise loc error = raise (Error { loc; error })
let type_pat = Type_pat.type_pat
let transl_type = Transl_type.transl_type
let make loc type_ desc = (type_, { expr_loc = loc; expr_desc = desc })

(* TODO: maybe type_x functions should return x * type? *)

(* TODO: likely type_expect? *)
let rec type_expr env term =
  let { s_loc = loc; s_desc = term } = term in
  let type_, term = type_desc ~loc env term in
  let type_ = generalize env type_ in
  (type_, term)

and type_desc ~loc env desc =
  (* dispatch to the proper function *)
  match desc with
  | S_ident name -> type_ident env ~loc ~name
  | S_number number -> type_number ~loc ~number
  | S_lambda { param; body } -> type_lambda env ~loc ~param ~body
  | S_apply { lambda; arg } -> type_apply env ~loc ~lambda ~arg
  | S_bind { bound; value; body } -> type_bind env ~loc ~bound ~value ~body
  | S_annot { value; type_ } -> type_annot env ~loc ~value ~type_
  | _ -> raise loc Unimplemented

and type_ident env ~loc ~name =
  let name = Name.make name in
  let ident, type_ = env |> Env.lookup loc name in
  make loc type_ (Expr_ident ident)

and type_number ~loc ~number =
  let number =
    match int_of_string_opt number with
    | Some number -> number
    | None -> raise loc Invalid_number
  in

  let type_ = new_int () in
  make loc type_ (Expr_number number)

and type_lambda env ~loc ~param ~body =
  let (param_type, param), env = type_pat env param in
  let body_type, body = type_expr env body in
  let type_ = new_arrow ~param:param_type ~return:body_type in
  make loc type_ (Expr_lambda { param; body })

and type_apply env ~loc ~lambda ~arg =
  let lambda_type, lambda = type_expr env lambda in

  let arg_type, arg = type_expr env arg in

  let return_type = new_weak_var () in
  let () =
    let expected = new_arrow ~param:arg_type ~return:return_type in
    unify ~loc ~expected ~received:lambda_type
  in

  make loc return_type (Expr_apply { lambda; arg })

and type_bind env ~loc ~bound ~value ~body =
  let value =
    match value with
    | Some value -> value
    | None -> raise loc Binding_without_value
  in
  let body =
    match body with Some body -> body | None -> raise loc Binding_without_body
  in
  (* a bind with body and value can be described as a let *)
  type_let env ~loc ~bound ~value ~body

and type_let env ~loc ~bound ~value ~body =
  (* typing: value first to prevent recursion *)
  let value_type, value = type_expr env value in
  let (bound_type, bound), env = type_pat env bound in
  let () = unify ~loc ~expected:bound_type ~received:value_type in

  let body_type, body = type_expr env body in
  make loc body_type (Expr_bind { bound; value; body })

and type_annot env ~loc ~value ~type_ =
  let value_type, value = type_expr env value in
  let type_type, type_ = transl_type env type_ in
  let () = unify ~loc ~expected:type_type ~received:value_type in

  make loc type_type (Expr_annot { value; type_ })
