type term = private ST of { loc : Location.t; desc : term_desc }

and term_desc = private
  | ST_var of { var : Name.t }
  | ST_arrow of { param : term; return : term }
  | ST_lambda of { param : term; return : term }
  | ST_apply of { lambda : term; arg : term }
  | ST_pair of { left : term; right : term }
  | ST_bind of { bound : term; value : term }
  | ST_semi of { left : term; right : term }
  | ST_annot of { value : term; type_ : term }
[@@deriving show]

val st_var : Location.t -> var:Name.t -> term
val st_arrow : Location.t -> param:term -> return:term -> term
val st_lambda : Location.t -> param:term -> return:term -> term
val st_apply : Location.t -> lambda:term -> arg:term -> term
val st_pair : Location.t -> left:term -> right:term -> term
val st_bind : Location.t -> bound:term -> value:term -> term
val st_semi : Location.t -> left:term -> right:term -> term
val st_annot : Location.t -> value:term -> type_:term -> term
