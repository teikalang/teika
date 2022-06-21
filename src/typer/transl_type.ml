open Utils
open Language
open Env
open Tree
open Type
open Transl_kind

let return_type env type_ desc =
  let loc = current_loc env in
  (type_, TT { env; loc; type_; desc })

let tt_var env type_ ~var = return_type env type_ (TT_var var)

let tt_forall env type_ ~var ~kind ~return =
  return_type env type_ (TT_forall { var; kind; return })

let tt_arrow env type_ ~param ~return =
  return_type env type_ (TT_arrow { param; return })

let tt_record env type_ ~fields = return_type env type_ (TT_record fields)

let tt_bind env field ~type_ ~var ~annot =
  let loc = current_loc env in
  (env, field, TT_bind { env; loc; type_; var; annot })

let rec transl_type env type_ =
  let (LT { loc; desc }) = type_ in
  let env = set_loc loc env in
  match desc with
  | LT_var name -> transl_type_var env ~name
  | LT_forall { var; kind; return } -> transl_type_forall env ~var ~kind ~return
  | LT_arrow { param; return } -> transl_type_arrow env ~param ~return
  | LT_record fields -> transl_type_record env ~fields

and transl_type_var env ~name =
  let name = Name.make name in
  let var, type_ = env |> Env.lookup name in
  tt_var env type_ ~var

and transl_type_forall env ~var ~kind ~return =
  (* TODO: use _kind_kind *)
  let _kind_kind, kind = transl_kind kind in

  let forall, env = enter_forall env in
  let var, env =
    let type_ = new_bound_var forall in
    let name = Name.make var in
    Env.add name type_ env
  in
  let return_type, return = transl_type env return in

  let type_ = new_forall forall ~return:return_type in
  tt_forall env type_ ~var ~kind ~return

and transl_type_arrow env ~param ~return =
  let param_type, param = transl_type env param in
  let return_type, return = transl_type env return in

  let type_ = new_arrow ~param:param_type ~return:return_type in
  tt_arrow env type_ ~param ~return

and transl_type_record env ~fields =
  let env, fields =
    List.fold_left_map
      (fun env field ->
        let env, field_type, field = transl_type_field env ~field in
        (env, (field_type, field)))
      env fields
  in
  let fields_type, fields = List.split fields in
  let type_ = new_record ~fields:fields_type in
  tt_record env type_ ~fields

and transl_type_field env ~field =
  let (LT_bind { loc; var; annot }) = field in
  let env = set_loc loc env in

  let name = Name.make var in
  match annot with
  | LA_kind kind ->
      (* TODO: use _kind_kind *)
      let _kind_kind, kind = transl_kind kind in
      let forall, env = enter_forall env in
      let type_ =
        let var = new_bound_var forall in
        new_type ~type_:var
      in
      let var, env = Env.add name type_ env in

      let field = new_field ~forall:(Some forall) ~name ~type_ in
      let annot = TA_kind kind in
      tt_bind env field ~type_ ~var ~annot
  | LA_type type_ ->
      let type_type, type_ = transl_type env type_ in
      let var, env = Env.add name type_type env in
      let field = new_field ~forall:None ~name ~type_:type_type in
      let annot = TA_type type_ in
      tt_bind env field ~type_:type_type ~var ~annot
