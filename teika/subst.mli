open Ttree
open Context

val subst_term : term -> term Subst_context.t
val subst_type : type_ -> type_ Subst_context.t
val subst_desc : term_desc -> term_desc Subst_context.t
val subst_annot : annot -> annot Subst_context.t
val subst_bind : bind -> bind Subst_context.t
