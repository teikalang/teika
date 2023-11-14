type term =
  | LT_loc of { term : term; loc : Location.t }
  (* x *)
  | LT_var of { var : Name.t }
  (* @x(e) *)
  | LT_extension of { extension : Name.t; payload : term }
  (* (x : A) -> (z : B) *)
  | LT_forall of { param : pat; return : term }
  (* (x : A) => e *)
  | LT_lambda of { param : pat; return : term }
  (* l a *)
  | LT_apply of { lambda : term; arg : term }
  (* x @-> m *)
  | LT_self of { self : pat; body : term }
  (* (@T : A) @=> m *)
  | LT_fix of { self : pat; body : term }
  (* @m *)
  | LT_unroll of { term : term }
  (* p = v; r *)
  | LT_let of { bound : bind; return : term }
  (* (v : T) *)
  | LT_annot of { term : term; annot : term }
  (* ".." *)
  | LT_string of { literal : string }

and pat =
  | LP_loc of { pat : pat; loc : Location.t }
  (* x *)
  | LP_var of { var : Name.t }
  (* @p *)
  | LP_unroll of { pat : pat }
  (* (p : T) *)
  | LP_annot of { pat : pat; annot : term }

and bind = LBind of { loc : Location.t; pat : pat; value : term }
[@@deriving show]
