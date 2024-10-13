open Utils

(* TODO: TT_with_type and TT_with_sort is not needed in many positions *)
(* TODO: subst to_ typed and check every time it is substituted *)
(* TODO: subst should have name *)

type term = private
  (* (M : A) *)
  | TT_with_type of { term : term; type_ : term }
  (* Type *)
  | TT_univ
  (* M#[x = N]*)
  | TT_subst of { term : term; to_ : term }
  (* M#l*)
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

and pat = private
  (* (P : A) *)
  | TP_with_type of { pat : pat; type_ : term }
  (* (P : A) *)
  | TP_annot of { pat : pat; annot : term }
  (* x *)
  | TP_var of { name : Name.t }
[@@deriving show]

(* assert *)
val assert_is_tt_with_type : term -> unit
val assert_is_tt_syntax : term -> unit
val assert_is_tp_with_type : pat -> unit
val assert_is_tp_syntax : pat -> unit

(* term header *)
val tt_with_type : type_:term -> term -> term
val tt_univ : term

(* term meta *)
val tt_subst : to_:term -> term -> term
val tt_shift : to_:Level.t -> term -> term

(* term syntax *)
val tt_annot : term:term -> annot:term -> term
val tt_var : name:Name.t -> var:Level.t -> term
val tt_forall : param:pat -> return:term -> term
val tt_lambda : param:pat -> return:term -> term
val tt_apply : lambda:term -> arg:term -> term
val tt_let : bound:pat -> value:term -> return:term -> term
val tt_string : literal:string -> term

(* pat header *)
val tp_with_type : type_:term -> pat -> pat

(* pat syntax *)
val tp_annot : pat:pat -> annot:term -> pat
val tp_var : name:Name.t -> pat

(* constants *)
val level_type_univ : Level.t
val level_type_string : Level.t
val tt_var_type_univ : term
val tt_var_type_string : term
