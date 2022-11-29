type term =
  (* x *)
  | LT_var of { var : Name.t }
  (* (x : A) -> (z : B) *)
  | LT_forall of { param : pat; return : term }
  (* (x : A) => e *)
  | LT_lambda of { param : pat; return : term }
  (* l a *)
  | LT_apply of { lambda : term; arg : term }
  (* (x : A, y : B) *)
  | LT_exists of { left : annot; right : annot }
  (* (x = 0, y = 0) *)
  | LT_pair of { left : bind; right : bind }
  (* p = v; r *)
  | LT_let of { bound : bind; return : term }
  (* (v : T) *)
  | LT_annot of { term : term; annot : term }
  | LT_loc of { term : term; loc : Location.t }

and pat =
  (* x *)
  | LP_var of { var : Name.t }
  (* (x, y) *)
  | LP_pair of { left : pat; right : pat }
  (* (p : T) *)
  | LP_annot of { pat : pat; annot : term }
  | LP_loc of { pat : pat; loc : Location.t }

(* TODO: rename to LAnnotation? *)
and annot = LAnnot of { loc : Location.t; pat : pat; annot : term }

and bind = LBind of { loc : Location.t; pat : pat; value : term }
[@@deriving show]
