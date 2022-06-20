open Language
open Tree
open Kind

let return_kind loc kind desc = (kind, TK { loc; desc })
let tk_type loc kind = return_kind loc kind TK_type

(* let tk_arrow loc kind ~param ~body =
   return_kind loc kind (TK_arrow { param; body }) *)

let rec transl_kind kind =
  let (LK { loc; desc = kind }) = kind in
  match kind with LK_type -> tk_type loc K_type
(* | LK_arrow { param; body } ->
    let param_kind, param = transl_kind param in
    let body_kind, body = transl_kind body in
    let kind = K_arrow { param = param_kind; return = body_kind } in
    tk_arrow loc kind ~param ~body *)
