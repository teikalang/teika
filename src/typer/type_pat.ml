open Syntax
open Tree
open Env
open Unify

type error = Unimplemented

exception Error of { loc : Location.t; error : error }

let raise loc error = raise (Error { loc; error })
let transl_type = Transl_type.transl_type
let make loc type_ desc = (type_, { pat_loc = loc; pat_desc = desc })
let return env loc type_ desc = (make loc type_ desc, env)

let rec type_pat env term =
  let { s_desc = term; s_loc = loc } = term in
  match term with
  | S_ident name ->
      let name = Name.make name in
      let type_ = new_weak_var env in
      let ident, env = env |> Env.enter loc name type_ in
      return env loc type_ (Pat_ident ident)
  | S_annot { value; type_ } ->
      let (pat_type, value), env = type_pat env value in
      let constraint_type, constraint_ = transl_type env type_ in

      let () = unify ~loc env ~expected:pat_type ~received:constraint_type in
      return env loc constraint_type (Pat_annot { pat = value; constraint_ })
  | _ -> raise loc Unimplemented
