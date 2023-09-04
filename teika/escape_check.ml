open Ttree
open Context
open Var_context
open Expand_head

let rec tt_escape_check ~current term =
  let tt_escape_check term = tt_escape_check ~current term in
  let tpat_escape_check pat = tpat_escape_check ~current pat in

  (* TODO: check without expand_head? *)
  (* TODO: this should not be here *)
  match tt_match @@ tt_expand_head term with
  | TT_subst { term; subst } -> tt_escape_check @@ tt_expand_subst ~subst term
  | TT_bound_var { index = _ } ->
      (* TODO: also check bound var *)
      (* TODO: very very important to check for bound vars, unification
            may unify variables outside of their binders *)
      pure ()
  | TT_free_var { level; alias = _ } -> (
      match Level.(current < level) with
      | true -> error_var_escape ~var:level
      | false -> pure ())
  | TT_hole _hole -> pure ()
  | TT_forall { param; return } ->
      let* () = tpat_escape_check param in
      tt_escape_check return
  | TT_lambda { param; return } ->
      let* () = tpat_escape_check param in
      tt_escape_check return
  | TT_apply { lambda; arg } ->
      let* () = tt_escape_check lambda in
      tt_escape_check arg
  | TT_self { var = _; body } -> tt_escape_check body
  | TT_fix { var = _; body } -> tt_escape_check body
  | TT_unroll { term } -> tt_escape_check term
  | TT_unfold { term } -> tt_escape_check term
  | TT_let { bound; value; return } ->
      let* () = tpat_escape_check bound in
      let* () = tt_escape_check value in
      tt_escape_check return
  | TT_annot { term; annot } ->
      let* () = tt_escape_check term in
      tt_escape_check annot
  | TT_string { literal = _ } -> pure ()
  | TT_native { native = _ } -> pure ()

and tpat_escape_check ~current term =
  (* TODO: check pat? *)
  let (TPat { pat = _; type_ }) = term in
  tt_escape_check ~current type_

let tt_escape_check term =
  let* current = level () in
  tt_escape_check ~current term
