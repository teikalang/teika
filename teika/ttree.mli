open Utils

type term =
  (* (M : A) *)
  | T_annot of { term : term; annot : term }
  (* \n *)
  | T_var of { var : Index.t }
  (* P = N; M *)
  | T_let of { bound : pat; arg : term; body : term }
  (* P => M *)
  | T_lambda of { bound : pat; body : term }
  (* M N *)
  | T_apply of { funct : term; arg : term }
  (* (P : A) -> B *)
  | T_forall of { bound : pat; param : term; body : term }
  (* (P : A) & B *)
  | T_inter of { bound : pat; left : term; right : term }

and pat =
  (* (P : A) *)
  | P_annot of { pat : pat; annot : term }
  (* x *)
  | P_var of { var : Name.t }
[@@deriving show]

(* TODO: write docs for this *)
type value = private
  (* equality *)
  | V_var of { at : Level.t }
  (* functions *)
  | V_apply of { funct : value; arg : value }
  | V_lambda of { env : env; body : term }
  (* types *)
  | V_univ
  | V_forall of { param : value; env : env; body : term }
  | V_inter of { left : value; env : env; right : term }
  (* laziness *)
  | V_thunk of { thunk : value Lazy.t }

and env [@@deriving show]

val empty : env
val v_univ : value
val skolem : at:Level.t -> value
val append : env -> value -> env
val shift : env -> by:Index.t -> env
val thunk : env -> term -> value
val eval : env -> term -> value
val head : value -> value
