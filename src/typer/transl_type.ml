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
  | S_lambda { param; body } -> transl_lambda ~loc env ~param ~body
  | _ -> raise loc Unimplemented

and transl_ident ~loc env ~name =
  let name = Name.make name in
  let ident, type_ = env |> Env.lookup loc name in
  (make loc type_ (Type_ident ident), env)

and transl_lambda ~loc env ~param ~body =
  match param.s_desc with
  (* TODO: what if it's None? *)
  | S_struct (Some param) -> transl_implicit_lambda ~loc env ~param ~body
  | _ -> transl_explicit_lambda ~loc env ~param ~body

and transl_implicit_lambda ~loc env ~param ~body =
  let open Type in
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
        let _ident, env = env |> Env.enter loc name type_ in
        env
    | _ -> raise loc Unimplemented
  in
  let (body_type, body), env = transl_type env body in
  let type_ = new_forall forall ~body:body_type in
  (make loc type_ (Type_implicit_lambda { body }), env)

and transl_explicit_lambda ~loc env ~param ~body =
  let (param_type, param), env = transl_type env param in
  let (body_type, body), env = transl_type env body in
  let type_ = new_arrow ~param:param_type ~return:body_type in
  (make loc type_ (Type_explicit_lambda { param; body }), env)

let transl_type env term =
  (* TODO: use this env??? *)
  let result, _env = transl_type env term in
  result
