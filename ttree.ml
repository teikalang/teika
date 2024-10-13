open Utils

type term =
  (* (M : A) *)
  (* TODO: store both expected and received types? *)
  | TT_with_type of { term : term; type_ : term }
  (* Type *)
  | TT_univ
  (* M[N]*)
  | TT_subst of { term : term; to_ : term }
  (* M#[shift l]*)
  | TT_shift of { term : term; to_ : Level.t }
  (* (M : A) *)
  | TT_annot of { term : term; annot : term }
  (* x *)
  | TT_var of { name : Name.t; var : Level.t }
  (* P -> B *)
  | TT_forall of { param : pat; return : term }
  (* P => M *)
  | TT_lambda of { param : pat; return : term }
  (* M N *)
  | TT_apply of { lambda : term; arg : term }
  (* P = N; M *)
  | TT_let of { bound : pat; value : term; return : term }
  (* ".." *)
  | TT_string of { literal : string }

and pat =
  (* (P : A) *)
  | TP_with_type of { pat : pat; type_ : term }
  (* (P : A) *)
  | TP_annot of { pat : pat; annot : term }
  (* x *)
  | TP_var of { name : Name.t }
[@@deriving show { with_path = false }]

(* asserts *)

let rec assert_is_tt_with_type term =
  match term with
  | TT_with_type _ | TT_univ -> ()
  | TT_subst { term; to_ = _ } -> assert_is_tt_with_type term
  | TT_shift { term; to_ = _ } -> assert_is_tt_with_type term
  | TT_annot _ | TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _ | TT_let _
  | TT_string _ ->
      assert false

let rec assert_is_tt_syntax term =
  match term with
  | TT_with_type _ -> assert false
  | TT_subst { term; to_ = _ } -> assert_is_tt_syntax term
  | TT_shift { term; to_ = _ } -> assert_is_tt_syntax term
  | TT_univ | TT_annot _ | TT_var _ | TT_forall _ | TT_lambda _ | TT_apply _
  | TT_let _ | TT_string _ ->
      ()

let assert_is_tp_with_type pat =
  match pat with TP_with_type _ -> () | TP_annot _ | TP_var _ -> assert false

let assert_is_tp_syntax pat =
  match pat with TP_annot _ | TP_var _ -> () | TP_with_type _ -> assert false

(* terms *)
let tt_with_type ~type_ term =
  let () = assert_is_tt_syntax term in
  let () = assert_is_tt_with_type type_ in
  TT_with_type { term; type_ }

let tt_univ = TT_univ

let tt_subst ~to_ term =
  let () = assert_is_tt_with_type term in
  let () = assert_is_tt_with_type to_ in
  TT_subst { term; to_ }

let tt_shift ~to_ term =
  let () = assert_is_tt_with_type term in
  TT_shift { term; to_ }

let tt_annot ~term ~annot =
  let () = assert_is_tt_with_type term in
  let () = assert_is_tt_with_type annot in
  TT_annot { term; annot }

let tt_var ~name ~var = TT_var { name; var }

let tt_forall ~param ~return =
  let () = assert_is_tp_with_type param in
  let () = assert_is_tt_with_type return in
  TT_forall { param; return }

let tt_lambda ~param ~return =
  let () = assert_is_tp_with_type param in
  let () = assert_is_tt_with_type return in
  TT_lambda { param; return }

let tt_apply ~lambda ~arg =
  let () = assert_is_tt_with_type lambda in
  let () = assert_is_tt_with_type arg in
  TT_apply { lambda; arg }

let tt_let ~bound ~value ~return =
  let () = assert_is_tp_with_type bound in
  let () = assert_is_tt_with_type value in
  let () = assert_is_tt_with_type return in
  TT_let { bound; value; return }

let tt_string ~literal = TT_string { literal }

(* patterns *)
let tp_with_type ~type_ pat =
  let () = assert_is_tp_syntax pat in
  let () = assert_is_tt_with_type type_ in
  TP_with_type { pat; type_ }

let tp_annot ~pat ~annot =
  let () = assert_is_tp_with_type pat in
  let () = assert_is_tt_with_type annot in
  TP_annot { pat; annot }

let tp_var ~name = TP_var { name }

(* constantas *)
let level_nil = Level.zero
let level_type_univ = Level.next level_nil
let level_type_string = Level.next level_type_univ

(* TODO: remove duplicated names *)
let tt_var_type_univ =
  tt_with_type ~type_:tt_univ
  @@ tt_var ~name:(Name.make "Type") ~var:level_type_univ

let tt_var_type_string =
  tt_with_type ~type_:tt_univ
  @@ tt_var ~name:(Name.make "String") ~var:level_type_string
