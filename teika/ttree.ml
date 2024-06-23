open Utils

type term =
  (* #(M : A) *)
  | TT_typed of { term : term; mutable type_ : term }
  (* (M : A) *)
  | TT_annot of { term : term; annot : term }
  (* \+n *)
  | TT_free_var of { var : Level.t }
  (* \-n *)
  | TT_bound_var of { var : Index.t }
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
  | TP_typed of { pat : pat; mutable type_ : term }
  (* (P : A) *)
  | TP_annot of { pat : pat; annot : term }
  (* x *)
  | TP_var of { name : Name.t }
[@@deriving show { with_path = false }]

(* TODO: expose this? *)
(* terms *)
let tt_typed ~type_ term = TT_typed { term; type_ }
let tt_annot ~term ~annot = TT_annot { term; annot }
let tt_free_var ~var = TT_free_var { var }
let tt_bound_var ~var = TT_bound_var { var }
let tt_forall ~param ~return = TT_forall { param; return }
let tt_lambda ~param ~return = TT_lambda { param; return }
let tt_apply ~lambda ~arg = TT_apply { lambda; arg }
let tt_let ~bound ~value ~return = TT_let { bound; value; return }
let tt_string ~literal = TT_string { literal }

(* patterns *)
let tp_typed ~type_ pat = TP_typed { pat; type_ }
let tp_annot ~pat ~annot = TP_annot { pat; annot }
let tp_var ~name = TP_var { name }

(* Nil *)
let level_nil = Level.zero
let tt_nil = TT_free_var { var = level_nil }

(* Type *)
let level_univ = Level.next level_nil
let tt_type_univ = TT_free_var { var = level_univ }

(* String *)
let level_string = Level.next level_univ

let tt_type_string =
  tt_typed ~type_:tt_type_univ @@ TT_free_var { var = level_string }
