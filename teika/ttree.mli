(* TODO: to avoid normalizing many times,
    makes normalization cached, with unification this
    means that this cache will probably be dependent on holes *)
(* TODO: make this private again *)
type term = TTerm of { loc : Location.t; desc : term_desc; type_ : type_ }
and type_ = TType of { loc : Location.t; desc : term_desc }

and term_desc =
  (* x *)
  | TT_var of { offset : Offset.t }
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
  (* x = v; r *)
  | TT_let of { bound : bind; return : term }
  (* (v : T) *)
  | TT_annot of { value : term; annot : type_ }
  (* e+-n *)
  | TT_offset of { desc : term_desc; offset : Offset.t }

and pat = TPat of { loc : Location.t; desc : pat_desc; type_ : type_ }

and pat_desc =
  (* x *)
  | TP_var of { var : Name.t }
  (* (p1, p2) *)
  | TP_pair of { left : pat; right : pat }
  (* (p : T) *)
  | TP_annot of { pat : pat; annot : type_ }

and annot = private TAnnot of { loc : Location.t; pat : pat; annot : type_ }
and bind = private TBind of { loc : Location.t; pat : pat; value : term }

(* term & type_*)
val tt_var : Location.t -> type_ -> offset:Offset.t -> term
val tt_forall : Location.t -> param:annot -> return:type_ -> type_
val tt_lambda : Location.t -> type_ -> param:annot -> return:term -> term
val tt_apply : Location.t -> type_ -> lambda:term -> arg:term -> term
val tt_exists : Location.t -> left:annot -> right:annot -> type_
val tt_pair : Location.t -> type_ -> left:bind -> right:bind -> term
val tt_let : Location.t -> type_ -> bound:bind -> return:term -> term
val tt_annot : Location.t -> value:term -> annot:type_ -> term

(* pattern *)
val tp_var : Location.t -> type_ -> var:Name.t -> pat
val tp_pair : Location.t -> type_ -> left:pat -> right:pat -> pat
val tp_annot : Location.t -> pat:pat -> annot:type_ -> pat

(* annot *)
val tannot : Location.t -> pat:pat -> annot:type_ -> annot

(* bind *)
val tbind : Location.t -> pat:pat -> value:term -> bind
