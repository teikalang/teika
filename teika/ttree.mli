(* TODO: make this private again? *)

type term =
  | TT_loc of { term : term; loc : Location.t }
  | TT_offset of { term : term; offset : Offset.t }
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

and pat =
  | TP_loc of { pat : pat; loc : Location.t }
  (* x *)
  | TP_var of { var : Name.t }
  (* (p : T) *)
  | TP_annot of { pat : pat; annot : term }
