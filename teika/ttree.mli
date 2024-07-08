open Utils

(* TODO: which invariants to enforce here using private? *)
(* TODO: return should be body *)

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
[@@deriving show]

(* term *)
val tterm : type_:term -> term_syntax -> term

(* TODO: drop those nil *)
val tterm_nil : term
val ttype : term_syntax -> term
val tt_annot : term:term -> annot:term -> term_syntax
val tt_free_var : var:Level.t -> term_syntax
val tt_bound_var : var:Index.t -> term_syntax
val tt_forall : param:pat -> return:term -> term_syntax
val tt_lambda : param:pat -> return:term -> term_syntax
val tt_apply : lambda:term -> arg:term -> term_syntax
val tt_let : bound:pat -> value:term -> return:term -> term_syntax
val tt_string : literal:string -> term_syntax

(* pat *)
val tpat : type_:term -> pat_syntax -> pat
val tpat_nil : pat
val tp_annot : pat:pat -> annot:term -> pat_syntax
val tp_var : name:Name.t -> pat_syntax

(* Type *)
val level_univ : Level.t
val tt_type_univ : term

(* String *)
val level_string : Level.t
val tt_type_string : term
