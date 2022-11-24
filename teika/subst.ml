open Ttree
module Subst_context = Context.Subst_context (Instance)
open Subst_context

(* TODO: also do substitution using mutation *)
let rec subst_term term =
  let (TTerm { loc; desc; type_ }) = term in
  let+ desc = subst_desc desc in
  TTerm { loc; desc; type_ }

and subst_type type_ =
  let (TType { loc; desc }) = type_ in
  let+ desc = subst_desc desc in
  TType { loc; desc }

and subst_annot : type a. _ -> (_ -> a subst_context) -> a subst_context =
 fun annot k ->
  let (TAnnot { loc; var; annot }) = annot in
  let* annot = subst_type annot in
  with_binder @@ fun () -> k (tannot loc ~var ~annot)

and subst_bind : type a. _ -> (_ -> a subst_context) -> a subst_context =
 fun bind k ->
  let (TBind { loc; var; value }) = bind in
  let* value = subst_term value in
  with_binder @@ fun () -> k (tbind loc ~var ~value)

and subst_desc desc =
  match desc with
  | TT_var { offset } -> (
      let* from = from () in
      let+ to_ = to_ () in
      match Offset.equal from offset with
      | true -> to_
      | false -> TT_var { offset })
  | TT_forall { param; return } ->
      subst_annot param @@ fun param ->
      let+ return = subst_type return in
      TT_forall { param; return }
  | TT_lambda { param; return } ->
      subst_annot param @@ fun param ->
      let+ return = subst_term return in
      TT_lambda { param; return }
  | TT_apply { lambda; arg } ->
      let* lambda = subst_term lambda in
      let+ arg = subst_term arg in
      TT_apply { lambda; arg }
  | TT_exists { left; right } ->
      subst_annot left @@ fun left ->
      subst_annot right @@ fun right -> return @@ TT_exists { left; right }
  | TT_pair { left; right } ->
      subst_bind left @@ fun left ->
      subst_bind right @@ fun right -> return @@ TT_pair { left; right }
  | TT_unpair { left; right; pair; return } ->
      let* pair = subst_term pair in
      with_binder @@ fun () ->
      with_binder @@ fun () ->
      let+ return = subst_term return in
      TT_unpair { left; right; pair; return }
  | TT_let { bound; return } ->
      subst_bind bound @@ fun bound ->
      let+ return = subst_term return in
      TT_let { bound; return }
  | TT_annot { value; annot } ->
      let* annot = subst_type annot in
      let+ value = subst_term value in
      TT_annot { value; annot }

let subst_annot annot = subst_annot annot @@ fun annot -> return @@ annot
let subst_bind bind = subst_bind bind @@ fun bind -> return @@ bind
