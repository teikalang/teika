type term = private LTerm of { loc : Location.t; desc : term_desc }

and term_desc = private
  (* x *)
  | LT_var of { var : Name.t }
  (* (x : A) -> (z : B) *)
  | LT_forall of { param : annot; return : term }
  (* (x : A) => e *)
  | LT_lambda of { param : annot; return : term }
  (* l a *)
  | LT_apply of { lambda : term; arg : term }
  (* (x : A, y : B) *)
  | LT_exists of { left : annot; right : annot }
  (* (x = 0, y = 0) *)
  | LT_pair of { left : bind; right : bind }
  (* (x, y) = v; r *)
  | LT_unpair of { left : Name.t; right : Name.t; pair : term; return : term }
  (* x = v; r *)
  | LT_let of { bound : bind; return : term }
  (* v : T *)
  | LT_annot of { value : term; type_ : term }

and annot = private LAnnot of { loc : Location.t; var : Name.t; type_ : term }

and bind = private LBind of { loc : Location.t; var : Name.t; value : term }
[@@deriving show]

(* term *)
val lt_var : Location.t -> var:Name.t -> term
val lt_forall : Location.t -> param:annot -> return:term -> term
val lt_lambda : Location.t -> param:annot -> return:term -> term
val lt_apply : Location.t -> lambda:term -> arg:term -> term
val lt_exists : Location.t -> left:annot -> right:annot -> term
val lt_pair : Location.t -> left:bind -> right:bind -> term

val lt_unpair :
  Location.t -> left:Name.t -> right:Name.t -> pair:term -> return:term -> term

val lt_let : Location.t -> bound:bind -> return:term -> term
val lt_annot : Location.t -> value:term -> type_:term -> term

(* annot *)
val lannot : Location.t -> var:Name.t -> type_:term -> annot

(* bind *)
val lbind : Location.t -> var:Name.t -> value:term -> bind
