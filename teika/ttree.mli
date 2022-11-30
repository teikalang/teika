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
  (* (x : A, y : B) *)
  | TT_exists of { left : annot; right : annot }
  (* (x = 0, y = 0) *)
  | TT_pair of { left : bind; right : bind }
  (* x = v; r *)
  | TT_let of { bound : bind; return : term }
  (* (v : T) *)
  | TT_annot of { term : term; annot : term }
  | TT_loc of { term : term; loc : Location.t }
  | TT_offset of { term : term; offset : Offset.t }

and pat =
  (* x *)
  | TP_var of { var : Name.t }
  (* (p1, p2) *)
  | TP_pair of { left : pat; right : pat }
  (* (p : T) *)
  | TP_annot of { pat : pat; annot : term }
  | TP_loc of { pat : pat; loc : Location.t }

and annot = TAnnot of { loc : Location.t; pat : pat; annot : term }

and bind = TBind of { loc : Location.t; pat : pat; value : term }
[@@deriving show]
