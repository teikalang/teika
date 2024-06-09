open Utils

type term =
  (* #(M : A) *)
  | TT_typed of { term : term; type_ : term }
  (* (M : A) *)
  | TT_annot of { term : term; annot : term }
  (* \!+n *)
  | TT_rigid_var of { var : Level.t }
  (* \+n *)
  | TT_global_var of { var : Level.t }
  (* \-n *)
  | TT_local_var of { var : Index.t }
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
  (* #(P : A) *)
  | TP_typed of { pat : pat; type_ : term }
  (* (P : A) *)
  | TP_annot of { pat : pat; annot : term }
  (* x *)
  | TP_var of { name : Name.t }
[@@deriving show { with_path = false }]

(* TODO: expose this? *)
(* terms *)
let tt_typed ~type_ term = TT_typed { term; type_ }
let tt_annot ~term ~annot = TT_annot { term; annot }
let tt_rigid_var ~var = TT_rigid_var { var }
let tt_global_var ~var = TT_global_var { var }
let tt_local_var ~var = TT_local_var { var }
let tt_forall ~param ~return = TT_forall { param; return }
let tt_lambda ~param ~return = TT_lambda { param; return }
let tt_apply ~lambda ~arg = TT_apply { lambda; arg }
let tt_let ~bound ~value ~return = TT_let { bound; value; return }
let tt_string ~literal = TT_string { literal }

(* patterns *)
let tp_typed ~type_ pat = TP_typed { pat; type_ }
let tp_annot ~pat ~annot = TP_annot { pat; annot }
let tp_var ~name = TP_var { name }

(* Type *)
let level_univ = Level.zero
let tt_rigid_univ = TT_rigid_var { var = level_univ }

(* String *)
let level_string = Level.next level_univ

let tt_rigid_string =
  tt_typed ~type_:tt_rigid_univ @@ TT_rigid_var { var = level_string }
