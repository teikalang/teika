open Context.Typer_context
open Ttree
open Expand_head

let rec escape_check : type a. current:_ -> a term -> _ =
 fun ~current term ->
  let escape_check term = escape_check ~current term in
  (* TODO: check without expand_head? *)
  match expand_head_term term with
  | TT_bound_var { index = _ } -> (* TODO: also check bound var *) return ()
  | TT_free_var { level } -> (
      match Level.(current < level) with
      | true -> error_var_escape ~var:level
      | false -> return ())
  | TT_hole _hole -> return ()
  | TT_forall { param; return } ->
      let* () = escape_check param in
      escape_check return
  | TT_lambda { param; return } ->
      let* () = escape_check param in
      escape_check return
  | TT_apply { lambda; arg } ->
      let* () = escape_check lambda in
      escape_check arg

let escape_check term =
  let* current = level () in
  escape_check ~current term
