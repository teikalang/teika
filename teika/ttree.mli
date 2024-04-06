open Utils

(* TODO: reduce allocations?
   TT_with_type and TT_with_sort is not needed in many positions *)
(* TODO: subst to_ typed and check every time it is substituted *)
(* TODO: which invariants to enforce here using private? *)

(* TODO: return should be body *)
type term = private
  (* #(M : A) *)
  | TTerm of { term : term_syntax; type_ : term }
  (* #(A : U) *)
  | TType of { term : term_syntax }

and term_syntax = private
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
  (* P = N; M *)
  | TT_let of { bound : pat; value : term; return : term }
  (* ".." *)
  | TT_string of { literal : string }

and pat = private
  (* #(P : A) *)
  | TPat of { pat : pat_syntax; type_ : term }

and pat_syntax =
  (* (P : A) *)
  | TP_annot of { pat : pat; annot : term }
  (* x *)
  | TP_var of { var : var }

and var = private
  | TVar of { name : Name.t; mutable link : term; mutable rename : var }
[@@deriving show]

(* invariants *)
exception TVar_already_linked of { var : var }
exception TVar_already_renamed of { var : var }

(* term *)
val tterm : type_:term -> term_syntax -> term
val ttype : term_syntax -> term
val tt_annot : term:term -> annot:term -> term_syntax
val tt_var : var:var -> term_syntax
val tt_forall : param:pat -> return:term -> term_syntax
val tt_lambda : param:pat -> return:term -> term_syntax
val tt_apply : lambda:term -> arg:term -> term_syntax
val tt_let : bound:pat -> value:term -> return:term -> term_syntax
val tt_string : literal:string -> term_syntax

(* pat *)
val tpat : type_:term -> pat_syntax -> pat
val tp_annot : pat:pat -> annot:term -> pat_syntax
val tp_var : var:var -> pat_syntax

(* var *)
val is_linked : var -> bool
val is_renamed : var -> bool
val is_tt_nil : term -> bool
val is_tv_nil : var -> bool
val tv_fresh : Name.t -> var

val with_tv_link : var -> to_:term -> (unit -> 'k) -> 'k
val with_tv_rename : var -> to_:var -> (unit -> 'k) -> 'k

(* Type *)
val tv_univ : var
val tt_global_univ : term

(* String *)
val tv_string : var
val tt_global_string : term
