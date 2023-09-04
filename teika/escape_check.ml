open Ttree
open Context
open Var_context
open Expand_head

let rec escape_check_term ~current term =
  let escape_check_term term = escape_check_term ~current term in
  let escape_check_typed_pat pat = escape_check_typed_pat ~current pat in

  (* TODO: check without expand_head? *)
  (* TODO: this should not be here *)
  match tt_match @@ expand_head_term term with
  | TT_subst { term; subst } ->
      escape_check_term @@ expand_subst_term ~subst term
  | TT_bound_var { index = _ } ->
      (* TODO: also check bound var *)
      (* TODO: very very important to check for bound vars, unification
            may unify variables outside of their binders *)
      return ()
  | TT_free_var { level; alias = _ } -> (
      match Level.(current < level) with
      | true -> error_var_escape ~var:level
      | false -> return ())
  | TT_hole _hole -> return ()
  | TT_forall { param; return } ->
      let* () = escape_check_typed_pat param in
      escape_check_term return
  | TT_lambda { param; return } ->
      let* () = escape_check_typed_pat param in
      escape_check_term return
  | TT_apply { lambda; arg } ->
      let* () = escape_check_term lambda in
      escape_check_term arg
  | TT_self { var = _; body } -> escape_check_term body
  | TT_fix { var = _; body } -> escape_check_term body
  | TT_unroll { term } -> escape_check_term term
  | TT_unfold { term } -> escape_check_term term
  | TT_let { bound; value; return } ->
      let* () = escape_check_typed_pat bound in
      let* () = escape_check_term value in
      escape_check_term return
  | TT_annot { term; annot } ->
      let* () = escape_check_term term in
      escape_check_term annot
  | TT_string { literal = _ } -> return ()
  | TT_native { native = _ } -> return ()

and escape_check_typed_pat ~current term =
  (* TODO: check pat? *)
  let (TPat { pat = _; type_ }) = term in
  escape_check_term ~current type_

let escape_check_term term =
  let* current = level () in
  escape_check_term ~current term
