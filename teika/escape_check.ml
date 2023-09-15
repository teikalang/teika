open Ttree
open Context
open Var_context
open Expand_head

(* TODO: duplicated *)
let open_term term =
  let open Var_context in
  let* level = level () in
  let to_ = TT_free_var { level } in
  let subst = TS_open { to_ } in
  pure @@ TT_subst { term; subst }

let rec tt_escape_check term =
  let* current = level () in
  (* TODO: check without expand_head? *)
  (* TODO: this should not be here *)
  match tt_match @@ tt_expand_head term with
  | TT_subst _ -> error_subst_found term
  (* TODO: also check bound var *)
  (* TODO: very very important to check for bound vars, unification
        may unify variables outside of their binders *)
  | TT_bound_var _ -> error_bound_var_found term
  | TT_unfold _ -> error_unfold_found term
  | TT_annot _ -> error_annot_found term
  | TT_free_var { level } -> (
      match Level.(current < level) with
      | true -> error_var_escape ~var:level
      | false -> pure ())
  | TT_hole _hole -> pure ()
  | TT_forall { param; return } | TT_lambda { param; return } ->
      let* () = tpat_escape_check param in
      with_free_var @@ fun () ->
      let* return = open_term return in
      tt_escape_check return
  | TT_apply { lambda; arg } ->
      let* () = tt_escape_check lambda in
      tt_escape_check arg
  | TT_self { var = _; body } | TT_fix { var = _; body } ->
      with_free_var @@ fun () ->
      let* body = open_term body in
      tt_escape_check body
  | TT_unroll { term } -> tt_escape_check term
  | TT_let { bound; value; return } ->
      let* () = tpat_escape_check bound in
      let* () = tt_escape_check value in
      with_free_var @@ fun () ->
      let* return = open_term return in
      tt_escape_check return
  | TT_string { literal = _ } -> pure ()
  | TT_native { native = _ } -> pure ()

and tpat_escape_check term =
  (* TODO: check pat? *)
  let (TPat { pat = _; type_ }) = term in
  tt_escape_check type_
