open Utils

type term =
  (* #(M : A) *)
  | TTerm of { term : term_syntax; type_ : term }
  (* #(A : U) *)
  | TType of { term : term_syntax }

and term_syntax =
  (* (M : A) *)
  | TT_annot of { term : term; annot : term }
  (* x *)
  | TT_var of { var : var }
  (* P -> B *)
  | TT_forall of { param : pat; return : term }
  (* P => M *)
  | TT_lambda of { param : pat; return : term }
  (* M N *)
  | TT_apply of { lambda : term; arg : term }
  (* P : A; M *)
  | TT_hoist of { bound : pat; annot : term; return : term }
  (* P = N; M *)
  | TT_let of { bound : pat; value : term; return : term }
  (* ".." *)
  | TT_string of { literal : string }

and pat = (* #(P : A) *)
  | TPat of { pat : pat_syntax; type_ : term }

and pat_syntax =
  (* (P : A) *)
  | TP_annot of { pat : pat; annot : term }
  (* x *)
  | TP_var of { var : var }

and var = TVar of { name : Name.t; mutable link : term; mutable rename : var }
[@@deriving show { with_path = false }]

(* TODO: expose this? *)
(* terms *)
let tterm ~type_ term = TTerm { term; type_ }
let ttype term = TType { term }
let tt_annot ~term ~annot = TT_annot { term; annot }
let tt_var ~var = TT_var { var }
let tt_forall ~param ~return = TT_forall { param; return }
let tt_lambda ~param ~return = TT_lambda { param; return }
let tt_apply ~lambda ~arg = TT_apply { lambda; arg }
let tt_hoist ~bound ~annot ~return = TT_hoist { bound; annot; return }
let tt_let ~bound ~value ~return = TT_let { bound; value; return }
let tt_string ~literal = TT_string { literal }

(* patterns *)
let tpat ~type_ pat = TPat { pat; type_ }
let tp_annot ~pat ~annot = TP_annot { pat; annot }
let tp_var ~var = TP_var { var }

(* constants *)
exception TVar_already_linked of { var : var }
exception TVar_already_renamed of { var : var }

let rec tt_nil = TTerm { term = TT_var { var = tv_nil }; type_ = tt_nil }

and tv_nil =
  let tv_nil_name = Name.make "teika_internal_nil" in
  TVar { name = tv_nil_name; link = tt_nil; rename = tv_nil }

let is_tt_nil term = term == tt_nil
let is_tv_nil var = var == tv_nil
let tv_fresh name = TVar { name; link = tt_nil; rename = tv_nil }

let is_linked var =
  let (TVar var_content) = var in
  not (is_tt_nil var_content.link)

let is_renamed var =
  let (TVar var_content) = var in
  not (is_tv_nil var_content.rename)

let assert_linked var =
  match is_linked var with
  | true -> ()
  | false -> raise (TVar_already_linked { var })

let assert_not_linked var =
  match is_linked var with
  | true -> raise (TVar_already_linked { var })
  | false -> ()

let tv_link var ~to_ =
  let (TVar var) = var in
  var.link <- to_

let tv_link_reset var =
  let () = assert_linked var in
  let (TVar var) = var in
  var.link <- tt_nil

let with_force_tv_link var ~to_ k =
  let () = tv_link var ~to_ in
  let value = k () in
  let () = tv_link_reset var in
  value

let with_tv_link var ~to_ k =
  let () = assert_not_linked var in
  with_force_tv_link var ~to_ k

let assert_renamed var =
  match is_renamed var with
  | true -> ()
  | false -> raise (TVar_already_renamed { var })

let assert_not_renamed var =
  match is_renamed var with
  | true -> raise (TVar_already_renamed { var })
  | false -> ()

let tv_rename var ~to_ =
  let () = assert_not_renamed var in
  let (TVar var) = var in
  var.rename <- to_

let tv_rename_reset var =
  let () = assert_renamed var in
  let (TVar var) = var in
  var.rename <- tv_nil

let with_tv_rename var ~to_ k =
  let () = tv_rename var ~to_ in
  let value = k () in
  let () = tv_rename_reset var in
  value

(* TODO: remove duplicated names *)
let tv_univ = tv_fresh (Name.make "Type")
let tt_global_univ = ttype @@ TT_var { var = tv_univ }
let tv_string = tv_fresh (Name.make "String")
let tt_global_string = ttype @@ TT_var { var = tv_string }
