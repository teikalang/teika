type term = private
  | TTerm of { loc : Location.t; desc : term_desc; type_ : type_ }

and type_ = private TType of { loc : Location.t; desc : term_desc }

and term_desc = private
  (* x *)
  | TT_var of { var : Var.t }
  (* (x : A) -> B *)
  | TT_forall of { param : annot; return : type_ }
  (* (x : A) => e *)
  | TT_lambda of { param : annot; return : term }
  (* l a *)
  | TT_apply of { lambda : term; arg : term }
  (* (x : A, y : B) *)
  | TT_exists of { left : annot; right : annot }
  (* (x = 0, y = 0) *)
  | TT_pair of { left : bind; right : bind }
  (* (x, y) = v; r *)
  | TT_unpair of { left : Var.t; right : Var.t; pair : term; return : term }
  (* x = v; r *)
  | TT_let of { bound : bind; return : term }
  (* v : T *)
  | TT_annot of { value : term; annot : type_ }

and annot = private TAnnot of { loc : Location.t; var : Var.t; annot : type_ }
and bind = private TBind of { loc : Location.t; var : Var.t; value : term }

(* term & type_*)
val tt_type : type_
val tt_var : Location.t -> type_ -> var:Var.t -> term
val tt_forall : Location.t -> param:annot -> return:type_ -> type_
val tt_lambda : Location.t -> type_ -> param:annot -> return:term -> term
val tt_apply : Location.t -> type_ -> lambda:term -> arg:term -> term
val tt_exists : Location.t -> left:annot -> right:annot -> type_
val tt_pair : Location.t -> type_ -> left:bind -> right:bind -> term

val tt_unpair :
  Location.t ->
  type_ ->
  left:Var.t ->
  right:Var.t ->
  pair:term ->
  return:term ->
  term

val tt_let : Location.t -> type_ -> bound:bind -> return:term -> term
val tt_annot : Location.t -> value:term -> annot:type_ -> term

(* annot *)
val tannot : Location.t -> var:Var.t -> annot:type_ -> annot

(* bind *)
val tbind : Location.t -> var:Var.t -> value:term -> bind

(* utils *)
exception Not_a_type of { term : term }

val term_of_type : type_ -> term
val type_of_term : term -> type_
