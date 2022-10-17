open Term

exception Var_clash of { expected : Var.t; received : Var.t }
exception Type_clash of { expected : term; received : term }
exception Not_a_function of { lambda : term }
exception Not_a_pair of { pair : term }

val normalize : term -> term
val equal : expected:term -> received:term -> unit
val typeof : term -> term
val apply : lambda:term -> arg:term -> unit
val pair : var:Var.t -> left:term -> right:term -> annot:term -> unit
val unpair : left:Var.t -> pair:term -> term * term
val annot : value:term -> type_:term -> unit
