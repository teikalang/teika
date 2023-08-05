type term = private ST of { loc : Location.t; desc : term_desc }

and term_desc = private
  | ST_var of { var : Name.t }
  | ST_extension of { extension : Name.t }
  | ST_forall of { param : term; return : term }
  | ST_lambda of { param : term; return : term }
  | ST_apply of { lambda : term; arg : term }
  | ST_pair of { left : term; right : term }
  | ST_both of { left : term; right : term }
  | ST_bind of { bound : term; value : term }
  | ST_semi of { left : term; right : term }
  | ST_annot of { value : term; annot : term }
  | ST_string of { literal : string }
  | ST_parens of { content : term }
  | ST_braces of { content : term }
[@@deriving show]

val st_var : Location.t -> var:Name.t -> term
val st_extension : Location.t -> extension:Name.t -> term
val st_forall : Location.t -> param:term -> return:term -> term
val st_lambda : Location.t -> param:term -> return:term -> term
val st_apply : Location.t -> lambda:term -> arg:term -> term
val st_pair : Location.t -> left:term -> right:term -> term
val st_both : Location.t -> left:term -> right:term -> term
val st_bind : Location.t -> bound:term -> value:term -> term
val st_semi : Location.t -> left:term -> right:term -> term
val st_annot : Location.t -> value:term -> annot:term -> term
val st_string : Location.t -> literal:string -> term
val st_parens : Location.t -> content:term -> term
val st_braces : Location.t -> content:term -> term
