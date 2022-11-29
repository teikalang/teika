type term = private LTerm of { loc : Location.t; desc : term_desc }

and term_desc = private
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
  | LT_annot of { value : term; annot : term }

and pat = private LPat of { loc : Location.t; desc : pat_desc }

and pat_desc = private
  (* x *)
  | LP_var of { var : Name.t }
  (* (x, y) *)
  | LP_pair of { left : pat; right : pat }
  (* (p : T) *)
  | LP_annot of { pat : pat; annot : term }

(* TODO: rename to LAnnotation? *)
and annot = private LAnnot of { loc : Location.t; pat : pat; annot : term }

and bind = private LBind of { loc : Location.t; pat : pat; value : term }
[@@deriving show]

(* term *)
val lt_var : Location.t -> var:Name.t -> term
val lt_forall : Location.t -> param:pat -> return:term -> term
val lt_lambda : Location.t -> param:pat -> return:term -> term
val lt_apply : Location.t -> lambda:term -> arg:term -> term
val lt_exists : Location.t -> left:annot -> right:annot -> term
val lt_pair : Location.t -> left:bind -> right:bind -> term
val lt_let : Location.t -> bound:bind -> return:term -> term
val lt_annot : Location.t -> value:term -> annot:term -> term

(* pattern *)
val lp_var : Location.t -> var:Name.t -> pat
val lp_pair : Location.t -> left:pat -> right:pat -> pat
val lp_annot : Location.t -> pat:pat -> annot:term -> pat

(* annot *)
val lannot : Location.t -> pat:pat -> annot:term -> annot

(* bind *)
val lbind : Location.t -> pat:pat -> value:term -> bind
