open Ttree
open Context
open Instance_context

(* TODO: also do substitution using mutation *)
let rec instance_term term =
  let (TTerm { loc; desc; type_ }) = term in
  let* type_ = instance_type type_ in
  let+ desc = instance_desc desc in
  TTerm { loc; desc; type_ }

and instance_type type_ =
  let (TType { loc; desc }) = type_ in
  let+ desc = instance_desc desc in
  TType { loc; desc }

and instance_annot :
    type a. _ -> (_ -> a instance_context) -> a instance_context =
 fun annot f ->
  let (TAnnot { loc; var; annot }) = annot in
  let* annot = instance_type annot in
  with_var @@ fun () -> f (tannot loc ~var ~annot)

and instance_bind : type a. _ -> (_ -> a instance_context) -> a instance_context
    =
 fun bind f ->
  let (TBind { loc; var; value }) = bind in
  let* value = instance_term value in
  with_var @@ fun () -> f (tbind loc ~var ~value)

and instance_desc desc =
  match desc with
  | TT_var { offset } -> repr_var ~var:offset
  | TT_forall { param; return } ->
      instance_annot param @@ fun param ->
      let+ return = instance_type return in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      instance_annot param @@ fun param ->
      let+ return = instance_term return in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let* lambda = instance_term lambda in
      let+ arg = instance_term arg in
      TT_apply { lambda; arg }
  | TT_exists { left; right } ->
      instance_annot left @@ fun left ->
      instance_annot right @@ fun right -> return @@ TT_exists { left; right }
  | TT_pair { left; right } ->
      instance_bind left @@ fun left ->
      instance_bind right @@ fun right -> return @@ TT_pair { left; right }
  | TT_unpair { left; right; pair; return } ->
      let* pair = instance_term pair in
      let+ return = instance_term return in
      TT_unpair { left; right; pair; return }
  | TT_let { bound; return } ->
      instance_bind bound @@ fun bound ->
      let+ return = instance_term return in
      TT_let { bound; return }
  | TT_annot { value; annot } ->
      let* annot = instance_type annot in
      let+ value = instance_term value in
      TT_annot { value; annot }
