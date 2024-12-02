open Utils

(* TODO: explicit unfold for loops on terms *)
type term = Term of { struct_ : term_struct; loc : Location.t [@opaque] }

and term_struct =
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
  | T_lambda of { bound : pat; body : term }
  (* M N *)
  | T_apply of { funct : term; arg : term }
  (* (P : A) -> B *)
  | T_forall of { bound : pat; param : term; body : term }
  (* (P : A) & B *)
  | T_self of { bound : var_pat; body : term }
  (* (M, ...) *)
  | T_tuple of { elements : term list }
  (* (x : A, ...) *)
  | T_exists of { elements : pat list }

and var_pat = VPat of { struct_ : var_pat_struct; loc : Location.t [@opaque] }

and var_pat_struct =
  (* (P : A) *)
  | VP_annot of { pat : var_pat; annot : term }
  (* x *)
  | VP_var of { var : Name.t }

and pat = Pat of { struct_ : pat_struct; loc : Location.t [@opaque] }

and pat_struct =
  (* (P : A) *)
  | P_annot of { pat : pat; annot : term }
  (* x *)
  (* TODO: drop names and uses receipts *)
  | P_var of { var : Name.t }
  (* (x, ...) *)
  | P_tuple of { elements : pat list }
[@@deriving show { with_path = false }]

let t_wrap ~loc struct_ = Term { struct_; loc }
let vp_wrap ~loc struct_ = VPat { struct_; loc }
let p_wrap ~loc struct_ = Pat { struct_; loc }
