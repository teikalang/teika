open Utils
open Syntax
open Type
open Tree

type error = Unimplemented

exception Error of { loc : Location.t; error : error }

let raise loc error = raise (Error { loc; error })
let make loc type_ desc = (type_, { type_loc = loc; type_desc = desc })

(* TODO: better way to do this? *)
let type_pat_ref = ref (fun _env _pat -> failwith "not initialized")
let _type_pat env pat = !type_pat_ref env pat

let rec transl_type env term =
  let { s_desc = term; s_loc = loc } = term in
  match term with
  | S_ident name -> transl_ident ~loc env ~name
  | S_arrow { param; body } -> transl_arrow ~loc env ~param ~body
  | S_struct content -> transl_struct ~loc env ~content
  | _ -> raise loc Unimplemented

and transl_ident ~loc env ~name =
  let name = Name.make name in
  let ident, type_ = env |> Env.lookup loc name in
  (make loc type_ (Type_ident ident), env)

and transl_arrow ~loc env ~param ~body =
  match param.s_desc with
  (* TODO: this is a hack, needs proper way to distinguish between pattern
     on structure and implicit *)
  | S_struct (Some ({ s_desc = S_ident _; s_loc = _ } as param)) ->
      transl_implicit_arrow ~loc env ~param ~body
  | _ -> transl_explicit_arrow ~loc env ~param ~body

and transl_implicit_arrow ~loc env ~param ~body =
  let forall = Forall_id.next () in
  let env =
    let { s_desc = param; s_loc = loc } = param in
    match param with
    | S_ident name ->
        let name = Name.make name in
        (* TODO: name for variables *)
        let type_ = new_bound_var ~name:None forall in
        (* TODO: shadowing? or duplicated name error? *)
        (* TODO: also this _ident, use it? *)
        let _ident, env = env |> Env.add loc name type_ in
        env
    | _ -> raise loc Unimplemented
  in
  let (body_type, body), env = transl_type env body in
  let type_ = new_forall forall ~body:body_type in
  (make loc type_ (Type_implicit_lambda { body }), env)

and transl_explicit_arrow ~loc env ~param ~body =
  let (param_type, param), env = transl_type env param in
  let (body_type, body), env = transl_type env body in
  let type_ = new_arrow ~param:param_type ~return:body_type in
  (make loc type_ (Type_explicit_lambda { param; body }), env)

and transl_struct ~loc env ~content =
  let (fields_type, fields), env =
    match content with
    | Some content -> transl_struct_content env ~content
    | None -> (([], []), env)
  in

  let type_ = new_struct ~fields:fields_type in
  (make loc type_ (Type_struct fields), env)

and transl_struct_content env ~content =
  let { s_desc = content; s_loc = loc } = content in
  match content with
  | S_bind { bound; value; body } ->
      (match value with Some _ -> raise loc Unimplemented | None -> ());
      let (field_type, field), env = transl_struct_field env ~bound in
      let (fields_type, fields), env =
        match body with
        | Some body -> transl_struct_content env ~content:body
        | None -> (([], []), env)
      in

      let fields_type = field_type :: fields_type in
      let fields = field :: fields in
      ((fields_type, fields), env)
  | _ -> raise loc Unimplemented

and transl_struct_field env ~bound =
  let { s_desc = bound; s_loc = loc } = bound in
  match bound with
  | S_annot { value; type_ } ->
      let name =
        let { s_desc = value; s_loc = loc } = value in
        match value with
        | S_ident name -> Name.make name
        | _ -> raise loc Unimplemented
      in
      let (type_, type_type), env = transl_type env type_ in
      let field_type = { name; type_ } in
      let field = { type_field_name = name; type_field_desc = type_type } in
      ((field_type, field), env)
  | _ -> raise loc Unimplemented

let transl_type env term =
  (* TODO: use this env??? *)
  let result, _env = transl_type env term in
  result
