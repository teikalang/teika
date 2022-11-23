open Ttree
open Context
open Instance_context

(* TODO: also do substitution using mutation *)
let rec instance_term term =
  let (TTerm { loc; desc; type_ }) = term in
  let+ desc = instance_desc desc in
  TTerm { loc; desc; type_ }

and instance_type type_ =
  let (TType { loc; desc }) = type_ in
  let+ desc = instance_desc desc in
  TType { loc; desc }

and instance_annot annot =
  let (TAnnot { loc; var; annot }) = annot in
  let+ annot = instance_type annot in
  tannot loc ~var ~annot

and instance_bind bind =
  let (TBind { loc; var; value }) = bind in
  let+ value = instance_term value in
  tbind loc ~var ~value

and instance_desc desc =
  match desc with
  | TT_var { offset = var_offset } ->
      let+ offset = offset () in
      let offset = Offset.(var_offset + offset) in
      TT_var { offset }
  | TT_forall { param; return } ->
      let* param = instance_annot param in
      let+ return = instance_type return in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      let* param = instance_annot param in
      let+ return = instance_term return in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let* lambda = instance_term lambda in
      let+ arg = instance_term arg in
      TT_apply { lambda; arg }
  | TT_exists { left; right } ->
      let* left = instance_annot left in
      let* right = instance_annot right in
      return @@ TT_exists { left; right }
  | TT_pair { left; right } ->
      let* left = instance_bind left in
      let* right = instance_bind right in
      return @@ TT_pair { left; right }
  | TT_unpair { left; right; pair; return } ->
      let* pair = instance_term pair in
      let+ return = instance_term return in
      TT_unpair { left; right; pair; return }
  | TT_let { bound; return } ->
      let* bound = instance_bind bound in
      let+ return = instance_term return in
      TT_let { bound; return }
  | TT_annot { value; annot } ->
      let* annot = instance_type annot in
      let+ value = instance_term value in
      TT_annot { value; annot }
