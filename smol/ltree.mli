type term = private LT of { loc : Location.t; desc : term_desc }

and term_desc = private
  (* * *)
  | LT_type
  (* x *)
  | LT_var of { var : Name.t }
  (* (x: a) -> b *)
  | LT_arrow of { var : Name.t; param : term; return : term }
  (* (x: a) => m *)
  | LT_lambda of { var : Name.t; param : term; return : term }
  (* (m n) *)
  | LT_apply of { lambda : term; arg : term }
  (* (x: a, b) *)
  | LT_sigma of { var : Name.t; left : term; right : term }
  (* (x = m, n : a) *)
  | LT_pair of { var : Name.t; left : term; right : term; annot : term }
  (* (x, y) = m; n *)
  | LT_unpair of { left : Name.t; right : Name.t; pair : term; return : term }
  (* x = m; n *)
  | LT_let of { var : Name.t; value : term; return : term }
  (* (m : a) *)
  | LT_annot of { value : term; type_ : term }
[@@deriving show]

val lt_type : Location.t -> term
val lt_var : Location.t -> var:Name.t -> term
val lt_arrow : Location.t -> var:Name.t -> param:term -> return:term -> term
val lt_lambda : Location.t -> var:Name.t -> param:term -> return:term -> term
val lt_apply : Location.t -> lambda:term -> arg:term -> term
val lt_sigma : Location.t -> var:Name.t -> left:term -> right:term -> term

val lt_pair :
  Location.t -> var:Name.t -> left:term -> right:term -> annot:term -> term

val lt_unpair :
  Location.t -> left:Name.t -> right:Name.t -> pair:term -> return:term -> term

val lt_let : Location.t -> var:Name.t -> value:term -> return:term -> term
val lt_annot : Location.t -> value:term -> type_:term -> term
