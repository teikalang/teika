type term =
  | CT_loc of { term : term; loc : Location.t }
  | CT_var of { var : Name.t }
  | CT_extension of { extension : Name.t }
  | CT_forall of { param : term; return : term }
  | CT_lambda of { param : term; return : term }
  | CT_apply of { lambda : term; arg : term }
  | CT_self of { self : term; body : term }
  | CT_fix of { self : term; body : term }
  | CT_unroll of { term : term }
  | CT_pair of { left : term; right : term }
  | CT_both of { left : term; right : term }
  | CT_bind of { bound : term; value : term }
  | CT_semi of { left : term; right : term }
  | CT_annot of { value : term; annot : term }
  | CT_string of { literal : string }
  | CT_parens of { content : term }
  | CT_braces of { content : term }
[@@deriving show]

val ct_var : Location.t -> var:Name.t -> term
val ct_extension : Location.t -> extension:Name.t -> term
val ct_forall : Location.t -> param:term -> return:term -> term
val ct_lambda : Location.t -> param:term -> return:term -> term
val ct_apply : Location.t -> lambda:term -> arg:term -> term
val ct_self : Location.t -> self:term -> body:term -> term
val ct_fix : Location.t -> self:term -> body:term -> term
val ct_unroll : Location.t -> term:term -> term
val ct_pair : Location.t -> left:term -> right:term -> term
val ct_both : Location.t -> left:term -> right:term -> term
val ct_bind : Location.t -> bound:term -> value:term -> term
val ct_semi : Location.t -> left:term -> right:term -> term
val ct_annot : Location.t -> value:term -> annot:term -> term
val ct_string : Location.t -> literal:string -> term
val ct_parens : Location.t -> content:term -> term
val ct_braces : Location.t -> content:term -> term
