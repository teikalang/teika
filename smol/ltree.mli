type expr = LE of { loc : Location.t; desc : expr_desc }

and expr_desc =
  (* x *)
  | LE_var of { var : Name.t }
  (* (x: t) => e *)
  | LE_lambda of { var : Name.t; param : type_; return : expr }
  (* (x: k) => e *)
  | LE_forall of { var : Name.t; return : expr }
  (* e1 e2 *)
  | LE_apply of { funct : expr; arg : expr }
  (* (x = e1, y = e2) *)
  | LE_pair of { left : bind; right : bind }
  (* (x: k, y = e) *)
  | LE_exists of { var : Name.t; right : bind }
  (* (x; y) = e; e2 *)
  | LE_unpair of { unpair : unpair; return : expr }
  (* #t *)
  | LE_type of { type_ : type_ }
  (* x = e1; e2 *)
  | LE_let of { bind : bind; return : expr }
  (* (e : t) *)
  | LE_annot of { expr : expr; type_ : type_ }

and unpair =
  (* (x, y) = e *)
  | LU of { loc : Location.t; left : Name.t; right : Name.t; value : expr }

and bind = (* x = e *)
  | LB of { loc : Location.t; var : Name.t; value : expr }

and type_ = LT of { loc : Location.t; desc : type_desc }

and type_desc =
  (* x *)
  | LT_var of { var : Name.t }
  (* t -> t *)
  | LT_arrow of { param : type_; return : type_ }
  (* (x: k) -> t *)
  | LT_forall of { var : Name.t; return : type_ }
  (* (x: t, y: t) *)
  | LT_pair of { left : type_; right : type_ }
  (* (x: k, y: t) *)
  | LT_exists of { var : Name.t; right : type_ }

(* expr *)
val le_var : Location.t -> var:Name.t -> expr
val le_lambda : Location.t -> var:Name.t -> param:type_ -> return:expr -> expr
val le_forall : Location.t -> var:Name.t -> return:expr -> expr
val le_apply : Location.t -> funct:expr -> arg:expr -> expr
val le_pair : Location.t -> left:bind -> right:bind -> expr
val le_exists : Location.t -> var:Name.t -> right:bind -> expr
val le_unpair : Location.t -> unpair:unpair -> return:expr -> expr
val le_type : Location.t -> type_:type_ -> expr
val le_let : Location.t -> bind:bind -> return:expr -> expr
val le_annot : Location.t -> expr:expr -> type_:type_ -> expr

(* unpair *)
val lu : Location.t -> left:Name.t -> right:Name.t -> value:expr -> unpair

(* bind *)
val lb : Location.t -> var:Name.t -> value:expr -> bind

(* type *)
val lt_var : Location.t -> var:Name.t -> type_
val lt_arrow : Location.t -> param:type_ -> return:type_ -> type_
val lt_forall : Location.t -> var:Name.t -> return:type_ -> type_
val lt_pair : Location.t -> left:type_ -> right:type_ -> type_
val lt_exists : Location.t -> var:Name.t -> right:type_ -> type_
