open Utils

(* TODO: which invariants to enforce here using private? *)
(* TODO: return should be body *)

type term = private
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

and pat = private
  (* #(P : A) *)
  | TP_typed of { pat : pat; type_ : term }
  (* (P : A) *)
  | TP_annot of { pat : pat; annot : term }
  (* x *)
  | TP_var of { name : Name.t }
[@@deriving show]

(* term *)
val tt_typed : type_:term -> term -> term
val tt_annot : term:term -> annot:term -> term
val tt_rigid_var : var:Level.t -> term
val tt_global_var : var:Level.t -> term
val tt_local_var : var:Index.t -> term
val tt_forall : param:pat -> return:term -> term
val tt_lambda : param:pat -> return:term -> term
val tt_apply : lambda:term -> arg:term -> term
val tt_let : bound:pat -> value:term -> return:term -> term
val tt_string : literal:string -> term

(* pat *)
val tp_typed : type_:term -> pat -> pat
val tp_annot : pat:pat -> annot:term -> pat
val tp_var : name:Name.t -> pat

(* Type *)
val level_univ : Level.t
val tt_rigid_univ : term

(* String *)
val level_string : Level.t
val tt_rigid_string : term
