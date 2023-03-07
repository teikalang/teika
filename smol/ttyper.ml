open Ltree
open Ttree

(* TODO: remove all failwith *)
let rec pat_var : type a. a pat -> _ =
 fun pat ->
  match pat with
  | TP_loc { pat; loc = _ } -> pat_var pat
  | TP_typed { pat; type_ = _ } -> pat_var pat
  | TP_var { var } -> var

let rec subst_term : type a. from:_ -> to_:_ -> a term -> ex_term =
 fun ~from ~to_ term ->
  let subst_term term = subst_term ~from ~to_ term in
  let subst_pat pat = subst_pat ~from ~to_ pat in
  match term with
  | TT_loc { term; loc } ->
      let (Ex_term term) = subst_term term in
      Ex_term (TT_loc { term; loc })
  | TT_var { var } -> (
      match Var.equal var from with
      | true -> Ex_term to_
      | false -> Ex_term (TT_var { var }))
  | TT_forall { param; return } ->
      let param = subst_pat param in
      let (Ex_term return) =
        match Var.equal from (pat_var param) with
        | true -> Ex_term return
        | false -> subst_term return
      in
      Ex_term (TT_forall { param; return })
  | TT_lambda { param; return } ->
      let param = subst_pat param in
      let (Ex_term return) =
        match Var.equal from (pat_var param) with
        | true -> Ex_term return
        | false -> subst_term return
      in
      Ex_term (TT_lambda { param; return })
  | TT_apply { lambda; arg } ->
      let (Ex_term lambda) = subst_term lambda in
      let (Ex_term arg) = subst_term arg in
      Ex_term (TT_apply { lambda; arg })
  | TT_self { bound; body } ->
      let bound = subst_pat bound in
      let (Ex_term body) =
        match Var.equal from (pat_var bound) with
        | true -> Ex_term body
        | false -> subst_term body
      in
      Ex_term (TT_self { bound; body })
  | TT_fix { bound; body } ->
      let bound = subst_pat bound in
      let (Ex_term body) =
        match Var.equal from (pat_var bound) with
        | true -> Ex_term body
        | false -> subst_term body
      in
      Ex_term (TT_fix { bound; body })
  | TT_unroll { term } ->
      let (Ex_term term) = subst_term term in
      Ex_term (TT_unroll { term })

and subst_pat : type a. from:_ -> to_:_ -> a pat -> a pat =
 fun ~from ~to_ pat ->
  let subst_term term = subst_term ~from ~to_ term in
  let subst_pat pat = subst_pat ~from ~to_ pat in
  match pat with
  | TP_loc { pat; loc } ->
      let pat = subst_pat pat in
      TP_loc { pat; loc }
  | TP_typed { pat; type_ } ->
      let pat = subst_pat pat in
      let (Ex_term type_) = subst_term type_ in
      TP_typed { pat; type_ }
  | TP_var _ as pat -> pat

let rec expand_head : type a. a term -> _ =
 fun term ->
  match term with
  | TT_loc { term; loc = _ } -> expand_head term
  | TT_var _ as term -> term
  | TT_forall _ as term -> term
  | TT_lambda _ as term -> term
  | TT_apply { lambda; arg } -> (
      match expand_head lambda with
      | TT_lambda { param; return } ->
          let (Ex_term return) =
            subst_term ~from:(pat_var param) ~to_:arg return
          in
          expand_head return
      | lambda -> TT_apply { lambda; arg })
  | TT_self _ as term -> term
  | TT_fix _ as term -> term
  | TT_unroll { term } -> (
      match expand_head term with
      | TT_fix { bound; body } as term ->
          let (Ex_term return) =
            subst_term ~from:(pat_var bound) ~to_:term body
          in
          expand_head return
      | term -> TT_unroll { term })

let rename ~from ~to_ term =
  let to_ = TT_var { var = to_ } in
  subst_term ~from ~to_ term

let split_pat pat =
  let (TP_typed { pat; type_ }) = pat in
  (pat_var pat, Ex_term type_)

(* TODO: maybe Var.copy *)
let copy_var var = Var.create (Var.name var)

(* equal1 checks for physical equality
   equal2 does structural equality *)
let rec equal1 : type r e. received:r term -> expected:e term -> _ =
 fun ~received ~expected ->
  let received = expand_head received in
  let expected = expand_head expected in
  match expected == received with
  | true -> ()
  | false -> equal2 ~received ~expected

and equal2 ~received ~expected =
  match (received, expected) with
  | TT_var { var = received }, TT_var { var = expected } -> (
      match Var.equal received expected with
      | true -> ()
      | false -> failwith "var clash")
  | ( TT_forall { param = received_param; return = received_return },
      TT_forall { param = expected_param; return = expected_return } ) ->
      let received_var, Ex_term received_type = split_pat received_param in
      let expected_var, Ex_term expected_type = split_pat expected_param in
      (* TODO: is the checking of annotation needed? Maybe a flag? *)
      equal1 ~received:expected_type ~expected:received_type;
      equal1_alpha_rename ~received_var ~expected_var ~received:received_return
        ~expected:expected_return
  | ( TT_lambda { param = received_param; return = received_return },
      TT_lambda { param = expected_param; return = expected_return } ) ->
      let received_var, Ex_term received_type = split_pat received_param in
      let expected_var, Ex_term expected_type = split_pat expected_param in
      (* TODO: is the checking of annotation needed? Maybe a flag? *)
      equal1 ~received:expected_type ~expected:received_type;
      equal1_alpha_rename ~received_var ~expected_var ~received:received_return
        ~expected:expected_return
  | ( TT_apply { lambda = received_lambda; arg = received_arg },
      TT_apply { lambda = expected_lambda; arg = expected_arg } ) ->
      equal1 ~received:received_lambda ~expected:expected_lambda;
      equal1 ~received:received_arg ~expected:expected_arg
  | ( TT_self { bound = received_bound; body = received_body },
      TT_self { bound = expected_bound; body = expected_body } ) ->
      let received_var = pat_var received_bound in
      let expected_var = pat_var expected_bound in
      equal1_alpha_rename ~received_var ~expected_var ~received:received_body
        ~expected:expected_body
  | ( TT_fix { bound = received_bound; body = received_body },
      TT_fix { bound = expected_bound; body = expected_body } ) ->
      let received_var, Ex_term received_type = split_pat received_bound in
      let expected_var, Ex_term expected_type = split_pat expected_bound in
      equal1 ~received:expected_type ~expected:received_type;
      equal1_alpha_rename ~received_var ~expected_var ~received:received_body
        ~expected:expected_body
  | TT_unroll { term = received }, TT_unroll { term = expected } ->
      equal1 ~received ~expected
  | ( ( TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_self _ | TT_fix _
      | TT_unroll _ ),
      ( TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_self _ | TT_fix _
      | TT_unroll _ ) ) ->
      failwith "type clash"

and equal1_alpha_rename :
    type r e.
    received_var:_ -> expected_var:_ -> received:r term -> expected:e term -> _
    =
 fun ~received_var ~expected_var ~received ~expected ->
  (* TODO: explain cross alpha rename *)
  (* TODO: why not rename to single side? like received_var := expected_var *)
  let skolem_var = copy_var expected_var in
  let (Ex_term received) = rename ~from:received_var ~to_:skolem_var received in
  let (Ex_term received) = rename ~from:expected_var ~to_:skolem_var received in
  let (Ex_term expected) = rename ~from:received_var ~to_:skolem_var expected in
  let (Ex_term expected) = rename ~from:expected_var ~to_:skolem_var expected in
  equal1 ~received ~expected

let typeof_term term =
  let (TT_typed { term = _; type_ }) = term in
  Ex_term type_

let typeof_pat term =
  let (TP_typed { pat = _; type_ }) = term in
  Ex_term type_

let wrap_term type_ term = TT_typed { term; type_ }
let wrap_pat type_ pat = TP_typed { pat; type_ }
let tt_type = TT_var { var = Var.type_ }

module Context : sig
  type context
  type t = context

  val initial : context
  val enter_param : param:typed pat -> context -> context
  val enter_alias : bound:typed pat -> value:_ term -> context -> context
  val lookup : name:Name.t -> context -> ty_term option
end = struct
  type context = ty_term Name.Map.t
  type t = context

  let initial =
    let open Name.Map in
    add (Var.name Var.type_) (wrap_term tt_type tt_type) empty

  let enter_param ~param ctx =
    let var, Ex_term type_ = split_pat param in
    let name = Var.name var in
    let term = TT_typed { term = TT_var { var }; type_ } in
    Name.Map.add name term ctx

  let enter_alias ~bound ~value ctx =
    let var, Ex_term type_ = split_pat bound in
    let name = Var.name var in
    (* TODO: preserve aliasing on lookup *)
    let term = TT_typed { term = value; type_ } in
    Name.Map.add name term ctx

  let lookup ~name ctx = Name.Map.find_opt name ctx
end

let rec infer_term ctx term =
  match term with
  | LT_loc { term; loc } ->
      let (TT_typed { term; type_ }) = infer_term ctx term in
      wrap_term type_ @@ TT_loc { term; loc }
  | LT_var { var } -> (
      (* TODO: is instantiation needed here?
          Maybe a flag to make it even safer? *)
      match Context.lookup ~name:var ctx with
      | Some term -> term
      | None -> failwith "unknown variable")
  | LT_forall { param; return } ->
      let param = infer_pat ctx param in
      let (Ex_term return) =
        let ctx = Context.enter_param ~param ctx in
        check_type ctx return
      in
      wrap_term tt_type @@ TT_forall { param; return }
  | LT_lambda { param; return } ->
      (* TODO: this pattern appears also in check LT_lambda *)
      let param = infer_pat ctx param in
      let (TT_typed { term = return; type_ = return_type }) =
        let ctx = Context.enter_param ~param ctx in
        infer_term ctx return
      in
      let forall = TT_forall { param; return = return_type } in
      wrap_term forall @@ TT_lambda { param; return }
  | LT_apply { lambda; arg } -> (
      let (TT_typed { term = lambda; type_ = forall }) =
        infer_term ctx lambda
      in
      match forall with
      | TT_forall { param; return } ->
          let (Ex_term arg) =
            let (Ex_term expected) = typeof_pat param in
            check_term ctx arg ~expected
          in
          let (Ex_term type_) =
            let from = pat_var param in
            subst_term ~from ~to_:arg return
          in
          wrap_term type_ @@ TT_apply { lambda; arg }
      | _ -> failwith "not a function")
  | LT_self _ | LT_fix _ | LT_unroll _ -> failwith "not implemented"
  | LT_alias { bound; value; return } ->
      (* TODO: keep alias in typed tree as sugar *)
      let bound = infer_pat ctx bound in
      let (Ex_term value) =
        let (Ex_term expected) = typeof_pat bound in
        check_term ctx value ~expected
      in

      (* TODO: keep alias in typed tree as sugar *)
      let ctx = Context.enter_alias ~bound ~value ctx in
      infer_term ctx return
  | LT_annot { term; annot } ->
      let (Ex_term annot) = check_type ctx annot in
      let (Ex_term term) = check_term ctx term ~expected:annot in
      (* TODO: keep annot in typed tree as sugar *)
      wrap_term annot term

and check_term : type a. _ -> _ -> expected:a term -> _ =
 fun ctx term ~expected ->
  (* TODO: bad naming *)
  let wrap_term term = Ex_term term in
  match (term, expand_head expected) with
  | LT_loc { term; loc }, expected ->
      let (Ex_term term) = check_term ctx term ~expected in
      wrap_term @@ TT_loc { term; loc }
  (* TODO: add flag to disable propagation *)
  (* TODO: also add a flag for double check, first with propagation
      then generate a complete AST and run without propagation *)
  | ( LT_lambda { param = received_param; return = received_return },
      TT_forall { param = expected_param; return = expected_return } ) ->
      let expected_var, param =
        let expected_var, Ex_term expected = split_pat expected_param in
        (expected_var, check_pat ctx received_param ~expected)
      in
      let (Ex_term return) =
        let (Ex_term expected) =
          let received_var = pat_var param in
          rename ~from:expected_var ~to_:received_var expected_return
        in
        let ctx = Context.enter_param ~param ctx in
        check_term ctx received_return ~expected
      in
      wrap_term @@ TT_lambda { param; return }
  | term, expected ->
      let (TT_typed { term; type_ = received }) = infer_term ctx term in
      let () = equal1 ~received ~expected in
      wrap_term term

and check_type ctx term = check_term ctx term ~expected:tt_type

and infer_pat ctx pat =
  match pat with
  | LP_loc { pat; loc } ->
      let (TP_typed { pat; type_ }) = infer_pat ctx pat in
      wrap_pat type_ @@ TP_loc { pat; loc }
  | LP_var { var = _ } -> failwith "missing type annotation"
  | LP_annot { pat; annot } ->
      let (Ex_term annot) = check_type ctx annot in
      check_pat ctx pat ~expected:annot

and check_pat : type e. _ -> _ -> expected:e term -> _ =
 fun ctx pat ~expected ->
  match pat with
  | LP_loc { pat; loc } ->
      let (TP_typed { pat; type_ }) = check_pat ctx pat ~expected in
      wrap_pat type_ @@ TP_loc { pat; loc }
  | LP_var { var } ->
      let var = Var.create var in
      wrap_pat expected @@ TP_var { var }
  | LP_annot { pat; annot } ->
      let (Ex_term annot) = check_type ctx annot in
      let pat = check_pat ctx pat ~expected:annot in
      let () =
        (* TODO: put error messages clash on the annot *)
        equal1 ~received:annot ~expected
      in
      (* TODO: keep annot in typed tree as sugar *)
      pat
