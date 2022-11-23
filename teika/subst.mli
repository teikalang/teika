open Ttree

val subst_term : from:Var.t -> to_:term_desc -> term -> term
val subst_type : from:Var.t -> to_:term_desc -> type_ -> type_
val subst_desc : from:Var.t -> to_:term_desc -> term_desc -> term_desc
val subst_annot : from:Var.t -> to_:term_desc -> annot -> annot
val subst_bind : from:Var.t -> to_:term_desc -> bind -> bind
