(* TODO: make this private again? *)

type term =
  (* x *)
  | TT_var of { offset : Offset.t }
  (* (x : A) -> B *)
  | TT_forall of { param : pat; return : term }
  (* (x : A) => e *)
  | TT_lambda of { param : pat; return : term }
  (* l a *)
  | TT_apply of { lambda : term; arg : term }
  (* (v : T) *)
  | TT_annot of { term : term; annot : term }
  | TT_offset of { term : term; offset : Offset.t }
  | TT_loc of { term : term; loc : Location.t }

and pat =
  (* x *)
  | TP_var of { var : Name.t }
  (* (p : T) *)
  | TP_annot of { pat : pat; annot : term }
  | TP_loc of { pat : pat; loc : Location.t }
[@@deriving show]
