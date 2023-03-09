open Ltree
open Ttree

(* TODO: remove all failwith *)

let pat_var pat = match pat with TP_var { var } -> var
let ty_pat_var pat = match pat with TP_typed { pat; type_ = _ } -> pat_var pat

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
  | TT_unroll { term } -> (
      match expand_head term with
      | TT_fix { bound; body } ->
          expand_head @@ subst_term ~from:(ty_pat_var bound) ~to_:term body
      | _ -> TT_unroll { term })

let split_pat pat =
  let (TP_typed { pat; type_ }) = pat in
  (pat_var pat, type_)

(* TODO: maybe Var.copy *)
let copy_var var = Var.create (Var.name var)

(* equal1 checks for physical equality
   equal2 does structural equality *)
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
  | ( ( TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_self _ | TT_fix _
      | TT_unroll _ ),
      ( TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_self _ | TT_fix _
      | TT_unroll _ ) ) ->
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

let typeof_term term =
  let (TT_typed { term = _; type_ }) = term in
  type_

let typeof_pat term =
  let (TP_typed { pat = _; type_ }) = term in
  type_

let wrap_term type_ term = TT_typed { term; type_ }
let wrap_pat type_ pat = TP_typed { pat; type_ }
let tt_type = TT_var { var = Var.type_ }

module Context : sig
  type 'tag context
  type 'tag t = 'tag context

  val initial : ty_term context
  val enter_param : param:ty_pat -> 'tag context -> 'tag context
  val enter_alias : bound:ty_pat -> value:term -> 'tag context -> 'tag context
  val lookup : name:Name.t -> 'tag context -> 'tag option
  val self_mode : ty_term context -> term context
end = struct
  type 'tag mode = M_typed : ty_term mode | M_self : term mode
  type 'tag context = Context of { mode : 'tag mode; vars : 'tag Name.Map.t }
  type 'tag t = 'tag context

  let initial =
    let mode = M_typed in
    let vars =
      let open Name.Map in
      add (Var.name Var.type_) (wrap_term tt_type tt_type) empty
    in
    Context { mode; vars }

  let enter_param (type tag) ~param (ctx : tag context) =
    let (Context { mode; vars }) = ctx in
    let var, type_ = split_pat param in
    let name = Var.name var in
    let term = TT_var { var } in
    let term : tag =
      match mode with M_typed -> TT_typed { term; type_ } | M_self -> term
    in
    let vars = Name.Map.add name term vars in
    Context { mode; vars }

  let enter_alias (type tag) ~bound ~value (ctx : tag context) =
    let (Context { mode; vars }) = ctx in
    let var, type_ = split_pat bound in
    let name = Var.name var in
    (* TODO: preserve aliasing on lookup *)
    let term : tag =
      match mode with
      | M_typed -> TT_typed { term = value; type_ }
      | M_self -> value
    in
    let vars = Name.Map.add name term vars in
    Context { mode; vars }

  let lookup ~name ctx =
    let (Context { mode = _; vars }) = ctx in
    Name.Map.find_opt name vars

  let self_mode ctx =
    let (Context { mode = _; vars }) = ctx in
    let mode = M_self in
    (* TODO: O(n) *)
    let vars =
      Name.Map.map
        (fun ty_term ->
          let (TT_typed { term; type_ = _ }) = ty_term in
          term)
        vars
    in
    Context { mode; vars }
end

let rec infer_ty_term ctx term =
  match term with
  | LT_loc { term; loc = _ } ->
      (* TODO: use this location *)
      infer_ty_term ctx term
  | LT_var { var } -> (
      (* TODO: is instantiation needed here?
          Maybe a flag to make it even safer? *)
      match Context.lookup ~name:var ctx with
      | Some term -> term
      | None -> failwith "unknown variable")
  | LT_forall { param; return } ->
      let param = infer_ty_pat ctx param in
      let return =
        let ctx = Context.enter_param ~param ctx in
        check_type ctx return
      in
      wrap_term tt_type @@ TT_forall { param; return }
  | LT_lambda { param; return } ->
      (* TODO: this pattern appears also in check LT_lambda *)
      let param = infer_ty_pat ctx param in
      let (TT_typed { term = return; type_ = return_type }) =
        let ctx = Context.enter_param ~param ctx in
        infer_ty_term ctx return
      in
      let forall = TT_forall { param; return = return_type } in
      wrap_term forall @@ TT_lambda { param; return }
  | LT_apply { lambda; arg } -> (
      let (TT_typed { term = lambda; type_ = forall }) =
        infer_ty_term ctx lambda
      in
      match forall with
      | TT_forall { param; return } ->
          let arg =
            let expected = typeof_pat param in
            check_term ctx arg ~expected
          in
          let type_ = subst_term ~from:(ty_pat_var param) ~to_:arg return in
          wrap_term type_ @@ TT_apply { lambda; arg }
      | _ -> failwith "not a function")
  | LT_self _ | LT_fix _ | LT_unroll _ -> failwith "not implemented"
  | LT_alias { bound; value; return } ->
      (* TODO: keep alias in typed tree as sugar *)
      let bound = infer_ty_pat ctx bound in
      let value =
        let expected = typeof_pat bound in
        check_term ctx value ~expected
      in

      (* TODO: keep alias in typed tree as sugar *)
      let ctx = Context.enter_alias ~bound ~value ctx in
      infer_ty_term ctx return
  | LT_annot { term; annot } ->
      let annot = check_type ctx annot in
      let term = check_term ctx term ~expected:annot in
      (* TODO: keep annot in typed tree as sugar *)
      wrap_term annot term

and check_term ctx term ~expected =
  match (term, expand_head expected) with
  | LT_loc { term; loc = _ }, expected ->
      (* TODO: use this location *)
      check_term ctx term ~expected
  (* TODO: add flag to disable propagation *)
  (* TODO: also add a flag for double check, first with propagation
      then generate a complete AST and run without propagation *)
  | ( LT_lambda { param = received_param; return = received_return },
      TT_forall { param = expected_param; return = expected_return } ) ->
      let expected_var, param =
        let expected_var, expected = split_pat expected_param in
        (expected_var, check_ty_pat ctx received_param ~expected)
      in
      let return =
        let expected =
          let received_var = ty_pat_var param in
          rename_term ~from:expected_var ~to_:received_var expected_return
        in
        let ctx = Context.enter_param ~param ctx in
        check_term ctx received_return ~expected
      in
      TT_lambda { param; return }
  | term, expected ->
      let (TT_typed { term; type_ = received }) = infer_ty_term ctx term in
      let () = equal_term ~received ~expected in
      term

and check_type ctx term = check_term ctx term ~expected:tt_type

and infer_ty_pat ctx pat =
  match pat with
  | LP_loc { pat; loc = _ } ->
      (* TODO: use this location *)
      infer_ty_pat ctx pat
  | LP_var { var = _ } -> failwith "missing type annotation"
  | LP_annot { pat; annot } ->
      let annot = check_type ctx annot in
      check_ty_pat ctx pat ~expected:annot

and check_ty_pat ctx pat ~expected =
  match pat with
  | LP_loc { pat; loc = _ } ->
      (* TODO: use this location *)
      check_ty_pat ctx pat ~expected
  | LP_var { var } ->
      let var = Var.create var in
      wrap_pat expected @@ TP_var { var }
  | LP_annot { pat; annot } ->
      let annot = check_type ctx annot in
      let pat = check_ty_pat ctx pat ~expected:annot in
      let () =
        (* TODO: put error messages clash on the annot *)
        equal_term ~received:annot ~expected
      in
      (* TODO: keep annot in typed tree as sugar *)
      pat

and assume_term ctx term =
  match term with
  | LT_loc { term; loc = _ } ->
      (* TODO: use this location *)
      assume_term ctx term
  | LT_var { var } -> (
      (* TODO: is instantiation needed here?
          Maybe a flag to make it even safer? *)
      match Context.lookup ~name:var ctx with
      | Some term -> term
      | None -> failwith "unknown variable")
  | LT_forall { param; return } ->
      let param = assume_infer_pat ctx param in
      let return =
        let ctx = Context.enter_param ~param ctx in
        assume_term ctx return
      in
      TT_forall { param; return }
  | LT_lambda { param; return } ->
      (* TODO: this pattern appears also in check LT_lambda *)
      let param = assume_infer_pat ctx param in
      let return =
        let ctx = Context.enter_param ~param ctx in
        assume_term ctx return
      in
      TT_lambda { param; return }
  | LT_apply { lambda; arg } ->
      let lambda = assume_term ctx lambda in
      let arg = assume_term ctx arg in
      TT_apply { lambda; arg }
  | LT_self _ | LT_fix _ | LT_unroll _ -> failwith "not implemented"
  | LT_alias { bound; value; return } ->
      (* TODO: keep alias in typed tree as sugar *)
      let bound = assume_infer_pat ctx bound in
      let value = assume_term ctx value in

      (* TODO: keep alias in typed tree as sugar *)
      let ctx = Context.enter_alias ~bound ~value ctx in
      assume_term ctx return
  | LT_annot { term; annot = _ } -> assume_term ctx term

and assume_infer_pat ctx pat =
  match pat with
  | LP_loc { pat; loc = _ } ->
      (* TODO: use this location *)
      assume_infer_pat ctx pat
  | LP_var { var = _ } -> failwith "missing type annotation"
  | LP_annot { pat; annot } ->
      let type_ = assume_term ctx annot in
      let rec find_var pat =
        match pat with
        | LP_loc { pat; loc = _ } ->
            (* TODO: use this location *)
            find_var pat
        | LP_var { var } -> var
        | LP_annot { pat; annot = _ } -> find_var pat
      in

      let pat =
        let var = find_var pat in
        let var = Var.create var in
        TP_var { var }
      in
      TP_typed { pat; type_ }
