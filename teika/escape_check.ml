open Context.Typer_context
open Ttree
open Expand_head

let rec escape_check_term : type a. current:_ -> a term -> _ =
 fun ~current term ->
  let escape_check_term term = escape_check_term ~current term in
  let escape_check_param pat = escape_check_param ~current pat in
  (* TODO: check without expand_head? *)
  match expand_head_term term with
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
      let* () = escape_check_param param in
      escape_check_term return
  | TT_lambda { param; return } ->
      let* () = escape_check_param param in
      escape_check_term return
  | TT_apply { lambda; arg } ->
      let* () = escape_check_term lambda in
      escape_check_term arg
  | TT_self { var = _; body } -> escape_check_term body
  | TT_fix { var = _; body } -> escape_check_term body
  | TT_unroll { term } -> escape_check_term term

and escape_check_param ~current term =
  (* TODO: check pat? *)
  let (TP_typed { pat = _; annot }) = term in
  escape_check_term ~current annot

let escape_check_term term =
  let* current = level () in
  escape_check_term ~current term
