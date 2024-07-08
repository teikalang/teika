open Utils

type term =
  (* #(M : A) *)
  | TTerm of { term : term_syntax; mutable type_ : term }
  (* #(A : S) *)
  | TType of { term : term_syntax }

and term_syntax =
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

and pat = (* #(P : A) *)
  | TPat of { pat : pat_syntax; mutable type_ : term }

and pat_syntax =
  (* (P : A) *)
  | TP_annot of { pat : pat; annot : term }
  (* x *)
  | TP_var of { name : Name.t }
[@@deriving show { with_path = false }]

(* TODO: expose this? *)
(* terms *)
let tterm ~type_ term = TTerm { term; type_ }
let ttype term = TType { term }
let tt_annot ~term ~annot = TT_annot { term; annot }
let tt_free_var ~var = TT_free_var { var }
let tt_bound_var ~var = TT_bound_var { var }
let tt_forall ~param ~return = TT_forall { param; return }
let tt_lambda ~param ~return = TT_lambda { param; return }
let tt_apply ~lambda ~arg = TT_apply { lambda; arg }
let tt_let ~bound ~value ~return = TT_let { bound; value; return }
let tt_string ~literal = TT_string { literal }

(* patterns *)
let tpat ~type_ pat = TPat { pat; type_ }
let tp_annot ~pat ~annot = TP_annot { pat; annot }
let tp_var ~name = TP_var { name }

(* Nil *)
let level_nil = Level.zero
let tterm_nil = ttype @@ TT_free_var { var = level_nil }
let tpat_nil = tpat ~type_:tterm_nil @@ TP_var { name = Name.make "**nil**" }

(* Type *)
let level_univ = Level.next level_nil
let tt_type_univ = ttype @@ TT_free_var { var = level_univ }

(* String *)
let level_string = Level.next level_univ
let tt_type_string = ttype @@ TT_free_var { var = level_string }
