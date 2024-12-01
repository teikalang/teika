open Utils

type term =
  (* (M : A) *)
  | T_annot of { term : term; annot : term }
  (* \n *)
  | T_var of { var : Index.t }
  (* P = N; M *)
  | T_let of { bound : pat; arg : term; body : term }
  (* x : A; M *)
  | T_hoist of { bound : var_pat; body : term }
  (* x : A; ...; x = N; M *)
  | T_fix of { bound : var_pat; var : Index.t; arg : term; body : term }
  (* P => M *)
  | T_lambda of { param : pat; body : term }
  (* M N *)
  | T_apply of { funct : term; arg : term }
  (* (P : A) -> B *)
  | T_forall of { param : pat; body : term }
  (* (P : A) & B *)
  | T_self of { self : var_pat; body : term }
  (* (x = M; ...) *)
  | T_tuple of { elements : term list }
  (* (x : A, ...) *)
  | T_exists of { elements : pat list }
  (* -2 *)
  | T_int32 of { lit : int32 }
  (* "a" *)
  | T_string of { lit : string }

and var_pat =
  (* (P : A) *)
  | VP_annot of { pat : var_pat; annot : term }
  (* x *)
  | VP_var of { var : Name.t }

and pat =
  (* (P : A) *)
  | P_annot of { pat : pat; annot : term }
  (* x *)
  | P_var of { var : Name.t }
  (* (x, ...) *)
  | P_tuple of { elements : pat list }
[@@deriving show]
