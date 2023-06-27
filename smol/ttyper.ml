open Ttree

(* TODO: remove all failwith *)

(* utils *)
let pat_var pat = match pat with TP_var { var } -> var
let ty_pat_var pat = match pat with TP_typed { pat; type_ = _ } -> pat_var pat

let split_pat pat =
  let (TP_typed { pat; type_ }) = pat in
  (pat_var pat, type_)

(* TODO: maybe Var.copy *)
let copy_var var = Var.create (Var.name var)

let typeof_term term =
  let (TT_typed { term = _; type_ }) = term in
  type_

let typeof_pat term =
  let (TP_typed { pat = _; type_ }) = term in
  type_

let wrap_term type_ term = TT_typed { term; type_ }
let wrap_pat type_ pat = TP_typed { pat; type_ }
let tt_type = TT_var { var = Var.type_ }

module Machinery = struct
  let rec subst_term ~from ~to_ term =
    let subst_term term = subst_term ~from ~to_ term in
    let subst_ty_pat pat = subst_ty_pat ~from ~to_ pat in
    let subst_pat pat = subst_pat ~from ~to_ pat in
    match term with
    | TT_var { var } -> (
        match Var.equal var from with true -> to_ | false -> TT_var { var })
    | TT_forall { param; return } ->
        let param = subst_ty_pat param in
        let return =
          match Var.equal from (ty_pat_var param) with
          | true -> return
          | false -> subst_term return
        in
        TT_forall { param; return }
    | TT_lambda { param; return } ->
        let param = subst_ty_pat param in
        let return =
          match Var.equal from (ty_pat_var param) with
          | true -> return
          | false -> subst_term return
        in
        TT_lambda { param; return }
    | TT_apply { lambda; arg } ->
        let lambda = subst_term lambda in
        let arg = subst_term arg in
        TT_apply { lambda; arg }
    | TT_self { bound; body } ->
        let bound = subst_pat bound in
        let body =
          match Var.equal from (pat_var bound) with
          | true -> body
          | false -> subst_term body
        in
        TT_self { bound; body }
    | TT_fix { bound; body } ->
        let bound = subst_ty_pat bound in
        let body =
          match Var.equal from (ty_pat_var bound) with
          | true -> body
          | false -> subst_term body
        in
        TT_fix { bound; body }
    | TT_unroll { term } ->
        let term = subst_term term in
        TT_unroll { term }
    | TT_expand { term } ->
        let term = subst_term term in
        TT_expand { term }

  and subst_ty_pat ~from ~to_ pat : ty_pat =
    let subst_term term = subst_term ~from ~to_ term in
    let subst_pat pat = subst_pat ~from ~to_ pat in
    match pat with
    | TP_typed { pat; type_ } ->
        let pat = subst_pat pat in
        let type_ = subst_term type_ in
        TP_typed { pat; type_ }

  and subst_pat ~from:_ ~to_:_ pat = match pat with TP_var _ as pat -> pat

  let rename_term ~from ~to_ term =
    let to_ = TT_var { var = to_ } in
    subst_term ~from ~to_ term

  let rec expand_head term =
    match term with
    | TT_var _ as term -> term
    | TT_forall _ as term -> term
    | TT_lambda _ as term -> term
    | TT_apply { lambda; arg } -> (
        match expand_head lambda with
        | TT_lambda { param; return } ->
            expand_head @@ subst_term ~from:(ty_pat_var param) ~to_:arg return
        | _ -> TT_apply { lambda; arg })
    | TT_self _ as term -> term
    | TT_fix _ as term -> term
    | TT_unroll _ as term -> term
    | TT_expand { term } ->
        (* TODO: is this safe? *)
        expand_head term

  let rec equal_term ~received ~expected =
    let received = expand_head received in
    let expected = expand_head expected in
    match (received, expected) with
    | TT_var { var = received }, TT_var { var = expected } -> (
        match Var.equal received expected with
        | true -> ()
        | false -> failwith "var clash")
    | ( TT_forall { param = received_param; return = received_return },
        TT_forall { param = expected_param; return = expected_return } ) ->
        let received_var, received_type = split_pat received_param in
        let expected_var, expected_type = split_pat expected_param in
        (* TODO: is the checking of annotation needed? Maybe a flag? *)
        equal_term ~received:expected_type ~expected:received_type;
        equal_term_alpha_rename ~received_var ~expected_var
          ~received:received_return ~expected:expected_return
    | ( TT_lambda { param = received_param; return = received_return },
        TT_lambda { param = expected_param; return = expected_return } ) ->
        let received_var, received_type = split_pat received_param in
        let expected_var, expected_type = split_pat expected_param in
        (* TODO: is the checking of annotation needed? Maybe a flag? *)
        equal_term ~received:expected_type ~expected:received_type;
        equal_term_alpha_rename ~received_var ~expected_var
          ~received:received_return ~expected:expected_return
    | ( TT_apply { lambda = received_lambda; arg = received_arg },
        TT_apply { lambda = expected_lambda; arg = expected_arg } ) ->
        equal_term ~received:received_lambda ~expected:expected_lambda;
        equal_term ~received:received_arg ~expected:expected_arg
    | ( TT_self { bound = received_bound; body = received_body },
        TT_self { bound = expected_bound; body = expected_body } ) ->
        let received_var = pat_var received_bound in
        let expected_var = pat_var expected_bound in
        equal_term_alpha_rename ~received_var ~expected_var
          ~received:received_body ~expected:expected_body
    | ( TT_fix { bound = received_bound; body = received_body },
        TT_fix { bound = expected_bound; body = expected_body } ) ->
        let received_var, received_type = split_pat received_bound in
        let expected_var, expected_type = split_pat expected_bound in
        equal_term ~received:expected_type ~expected:received_type;
        equal_term_alpha_rename ~received_var ~expected_var
          ~received:received_body ~expected:expected_body
    | TT_unroll { term = received }, TT_unroll { term = expected } ->
        equal_term ~received ~expected
    | ( ( TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_self _
        | TT_fix _ | TT_unroll _ | TT_expand _ ),
        ( TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_self _
        | TT_fix _ | TT_unroll _ | TT_expand _ ) ) ->
        failwith "type clash"

  and equal_term_alpha_rename ~received_var ~expected_var ~received ~expected =
    (* TODO: explain cross alpha rename *)
    (* TODO: why not rename to single side? like received_var := expected_var *)
    let skolem_var = copy_var expected_var in
    let received = rename_term ~from:received_var ~to_:skolem_var received in
    let received = rename_term ~from:expected_var ~to_:skolem_var received in
    let expected = rename_term ~from:received_var ~to_:skolem_var expected in
    let expected = rename_term ~from:expected_var ~to_:skolem_var expected in
    equal_term ~received ~expected

  let rec expand_unroll term =
    (* TODO: this only expands shallow for convenience
        a more principled solution is possible *)
    (* TODO: why not expand_head? *)
    match term with
    | TT_var _ as term -> term
    | TT_forall _ as term -> term
    | TT_lambda _ as term -> term
    | TT_apply { lambda; arg } ->
        let lambda = expand_unroll lambda in
        let arg = expand_unroll arg in
        TT_apply { lambda; arg }
    | TT_self _ as term -> term
    | TT_fix _ as term -> term
    | TT_unroll { term } -> (
        match expand_head term with
        | TT_fix { bound; body } as fix ->
            subst_term ~from:(ty_pat_var bound) ~to_:fix body
        | _ -> TT_unroll { term })
    | TT_expand _ as term -> term
end

module Translate = struct
  open Ltree
  open Ttree

  module Context : sig
    type context
    type t = context

    val initial : context
    val enter : name:Name.t -> term -> context -> context
    val lookup : name:Name.t -> context -> term option
  end = struct
    type context = term Name.Map.t
    type t = context

    let initial =
      let open Name.Map in
      add (Var.name Var.type_) (TT_var { var = Var.type_ }) empty

    let enter ~name term ctx = Name.Map.add name term ctx
    let lookup ~name ctx = Name.Map.find_opt name ctx
  end

  let enter_param ~param ctx =
    let (TP_typed { pat; type_ = _ }) = param in
    let var = pat_var pat in
    let name = Var.name var in
    let term = TT_var { var } in
    Context.enter ~name term ctx

  let enter_self ~bound ctx =
    let var = pat_var bound in
    let name = Var.name var in
    let term = TT_var { var } in
    Context.enter ~name term ctx

  let enter_alias ~bound ~value ctx =
    let (TP_typed { pat; type_ = _ }) = bound in
    let var = pat_var pat in
    let name = Var.name var in
    Context.enter ~name value ctx

  let rec translate_term ctx term =
    match term with
    | LT_loc { term; loc = _ } ->
        (* TODO: use this location *)
        translate_term ctx term
    | LT_var { var } -> (
        match Context.lookup ~name:var ctx with
        | Some term -> term
        | None -> failwith "unknown variable")
    | LT_forall { param; return } ->
        let param = translate_ty_pat ctx param in
        let return =
          let ctx = enter_param ~param ctx in
          translate_term ctx return
        in
        TT_forall { param; return }
    | LT_lambda { param; return } ->
        (* TODO: this pattern appears also in check LT_lambda *)
        let param = translate_ty_pat ctx param in
        let return =
          let ctx = enter_param ~param ctx in
          translate_term ctx return
        in
        TT_lambda { param; return }
    | LT_apply { lambda; arg } ->
        let lambda = translate_term ctx lambda in
        let arg = translate_term ctx arg in
        TT_apply { lambda; arg }
    | LT_self { bound; body } ->
        let bound = translate_pat bound in
        let body =
          let ctx = enter_self ~bound ctx in
          translate_term ctx body
        in
        TT_self { bound; body }
    | LT_fix { bound; body } ->
        let bound = translate_ty_pat ctx bound in
        let body =
          (* TODO: bad naming *)
          let ctx = enter_param ~param:bound ctx in
          translate_term ctx body
        in
        TT_fix { bound; body }
    | LT_unroll { term } ->
        let term = translate_term ctx term in
        TT_unroll { term }
    | LT_expand { term } ->
        let term = translate_term ctx term in
        TT_expand { term }
    | LT_alias { bound; value; return } ->
        (* TODO: use annotation in bound  *)
        let bound = translate_ty_pat ctx bound in
        let value = translate_term ctx value in
        let ctx = enter_alias ~bound ~value ctx in
        translate_term ctx return
    | LT_annot { term; annot = _ } ->
        (* TODO: use annotation *)
        translate_term ctx term

  and translate_ty_pat ctx pat =
    match pat with
    | LP_loc { pat; loc = _ } ->
        (* TODO: use this location *)
        translate_ty_pat ctx pat
    | LP_var { var = _ } -> failwith "missing type annotation"
    | LP_annot { pat; annot } ->
        let type_ = translate_term ctx annot in
        let pat = translate_pat pat in
        TP_typed { pat; type_ }

  and translate_pat pat =
    match pat with
    | LP_loc { pat; loc = _ } ->
        (* TODO: use this location *)
        translate_pat pat
    | LP_var { var } ->
        let var = Var.create var in
        TP_var { var }
    | LP_annot { pat = _; annot = _ } -> failwith "unexpected type annotation"
end

module Context : sig
  type context
  type t = context

  val initial : context
  val enter : var:Var.t -> term -> context -> context
  val lookup : var:Var.t -> context -> term option
end = struct
  type context = term Var.Map.t
  type t = context

  let initial =
    let open Var.Map in
    add Var.type_ (TT_var { var = Var.type_ }) empty

  let enter ~var term ctx = Var.Map.add var term ctx
  let lookup ~var ctx = Var.Map.find_opt var ctx
end

open Machinery

let enter_param ~param ctx =
  let (TP_typed { pat; type_ }) = param in
  let var = pat_var pat in
  Context.enter ~var type_ ctx

let enter_self ~bound ~body ctx =
  let var = pat_var bound in
  let assumption = TT_self { bound; body } in
  Context.enter ~var assumption ctx

let enter_alias ~bound ctx =
  let (TP_typed { pat; type_ }) = bound in
  let var = pat_var pat in
  Context.enter ~var type_ ctx

let split_self term =
  match expand_head @@ term with
  | TT_self { bound; body } -> (bound, body)
  | _ -> failwith "not a self"

let rec infer_term ctx term =
  match term with
  | TT_var { var } -> (
      (* TODO: is instantiation needed here?
          Maybe a flag to make it even safer? *)
      match Context.lookup ~var ctx with
      | Some type_ -> type_
      | None -> failwith "unknown variable")
  | TT_forall { param; return } ->
      let () = check_ty_pat ctx param in
      let () =
        let ctx = enter_param ~param ctx in
        check_type ctx return
      in
      tt_type
  | TT_lambda { param; return } ->
      (* TODO: this pattern appears also in check LT_lambda *)
      let () = check_ty_pat ctx param in
      let return =
        let ctx = enter_param ~param ctx in
        infer_term ctx return
      in
      TT_forall { param; return }
  | TT_apply { lambda; arg } -> (
      match expand_head @@ infer_term ctx lambda with
      | TT_forall { param; return } ->
          let () =
            let expected = typeof_pat param in
            check_term ctx arg ~expected
          in
          subst_term ~from:(ty_pat_var param) ~to_:arg return
      | _ -> failwith "not a function")
  | TT_self { bound; body } ->
      let () =
        (* TODO: all patterns are valid? Not checking bound *)
        let ctx = enter_self ~bound ~body ctx in
        check_type ctx body
      in
      tt_type
  | TT_fix { bound; body } ->
      let () = check_ty_pat ctx bound in
      let body =
        let ctx = enter_param ~param:bound ctx in
        infer_term ctx body
      in
      let (TP_typed { pat; type_ = expected }) = bound in
      let received = TT_self { bound = pat; body } in
      let () = equal_term ~received ~expected in
      expected
  | TT_unroll { term } ->
      let bound, body = split_self @@ infer_term ctx term in
      subst_term ~from:(pat_var bound) ~to_:term body
  | TT_expand { term } -> expand_unroll @@ infer_term ctx term

and check_term ctx term ~expected =
  let received = infer_term ctx term in
  equal_term ~received ~expected

and check_type ctx term = check_term ctx term ~expected:tt_type

and check_ty_pat ctx pat =
  (* TODO: all patterns are valid? *)
  let (TP_typed { pat = _; type_ }) = pat in
  check_type ctx type_
