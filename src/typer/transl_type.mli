val type_pat_ref : (Env.t -> Syntax.term -> (Type.t * Tree.pat) * Env.t) ref
val transl_type : Env.t -> Syntax.term -> Type.t * Tree.type_
