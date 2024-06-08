open Utils

(* TODO: reduce allocations?
   TT_with_type and TT_with_sort is not needed in many positions *)
(* TODO: subst to_ typed and check every time it is substituted *)
(* TODO: which invariants to enforce here using private? *)

(* TODO: return should be body *)
(* TODO: cache normalization? *)
type term = private
  (* #(M : A) *)
  | TTerm of { term : term_syntax; type_ : term }
  (* #(A : U) *)
  | TType of { term : term_syntax }

and term_syntax = private
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

and pat = private TPat (* #(P : A) *) of { pat : pat_syntax; type_ : term }

and pat_syntax =
  (* (P : A) *)
  | TP_annot of { pat : pat; annot : term }
  (* x *)
  | TP_var of { name : Name.t }
[@@deriving show]

(* term *)
val tterm : type_:term -> term_syntax -> term
val ttype : term_syntax -> term
val tt_annot : term:term -> annot:term -> term_syntax
val tt_rigid_var : var:Level.t -> term_syntax
val tt_global_var : var:Level.t -> term_syntax
val tt_local_var : var:Index.t -> term_syntax
val tt_forall : param:pat -> return:term -> term_syntax
val tt_lambda : param:pat -> return:term -> term_syntax
val tt_apply : lambda:term -> arg:term -> term_syntax
val tt_let : bound:pat -> value:term -> return:term -> term_syntax
val tt_string : literal:string -> term_syntax

(* pat *)
val tpat : type_:term -> pat_syntax -> pat
val tp_annot : pat:pat -> annot:term -> pat_syntax
val tp_var : name:Name.t -> pat_syntax

(* Type *)
val level_univ : Level.t
val tt_global_univ : term

(* String *)
val level_string : Level.t
val tt_global_string : term
