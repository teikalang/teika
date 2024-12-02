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
  (* P : A; M *)
  | T_hoist of { bound : pat; body : term }
  (* P : A; ...; P = N; M *)
  | T_fix of { bound : pat; var : Index.t; arg : term; body : term }
  (* P => M *)
  | T_lambda of { bound : pat; body : term }
  (* M N *)
  | T_apply of { funct : term; arg : term }
  (* (P : A) -> B *)
  | T_forall of { bound : pat; param : term; body : term }
  (* TODO: part of fix *)
  (* (P : A) & B *)
  | T_self of { bound : pat; body : term }

and pat = Pat of { struct_ : pat_struct; loc : Location.t [@opaque] }

and pat_struct =
  (* (M : A) *)
  | P_annot of { pat : pat; annot : term }
  (* x *)
  (* TODO: drop names and uses receipts *)
  | P_var of { var : Name.t }
[@@deriving show { with_path = false }]

let t_wrap ~loc struct_ = Term { struct_; loc }
let p_wrap ~loc struct_ = Pat { struct_; loc }
