module Name = Name

type delimiter = Parens | Brackets

type term = CTerm of { term : term_syntax; loc : Location.t }

and term_syntax =
  | CT_hole
  | CT_var of { var : Name.t }
  | CT_unit of { delimiter : delimiter }
  | CT_apply of { funct : term; arg : term }
  | CT_pi of { bound : term; body : term }
  | CT_lambda of { bound : term; body : term }
  | CT_self of { bound : term; body : term }
  | CT_fix of { bound : term; body : term }
  | CT_unfold of { fix : term }
  | CT_fst of { pair : term }
  | CT_snd of { pair : term }
  | CT_pair of { left : term; right : term }
  | CT_inter of { left : term; right : term }
  | CT_union of { left : term; right : term }
  | CT_bind of { bound : term; value : term }
  | CT_semi of { left : term; right : term }
  | CT_annot of { value : term; annot : term }
  | CT_string of { literal : string }
  | CT_number of { literal : Z.t }
  | CT_parens of { content : term }
  | CT_brackets of { content : term }
  | CT_braces of { content : term }

val ct_hole : Location.t -> term
val ct_var : Location.t -> var:Name.t -> term
val ct_unit : Location.t -> delimiter:delimiter -> term
val ct_apply : Location.t -> funct:term -> arg:term -> term
val ct_pi : Location.t -> bound:term -> body:term -> term
val ct_lambda : Location.t -> bound:term -> body:term -> term
val ct_self : Location.t -> bound:term -> body:term -> term
val ct_fix : Location.t -> bound:term -> body:term -> term
val ct_unfold : Location.t -> fix:term -> term
val ct_fst : Location.t -> pair:term -> term
val ct_snd : Location.t -> pair:term -> term
val ct_pair : Location.t -> left:term -> right:term -> term
val ct_inter : Location.t -> left:term -> right:term -> term
val ct_union : Location.t -> left:term -> right:term -> term
val ct_bind : Location.t -> bound:term -> value:term -> term
val ct_semi : Location.t -> left:term -> right:term -> term
val ct_annot : Location.t -> value:term -> annot:term -> term
val ct_string : Location.t -> literal:string -> term
val ct_number : Location.t -> literal:Z.t -> term
val ct_parens : Location.t -> content:term -> term
val ct_brackets : Location.t -> content:term -> term
val ct_braces : Location.t -> content:term -> term
