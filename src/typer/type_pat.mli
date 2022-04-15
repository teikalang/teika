(* a pattern may introduce new names in the environment,
   that's why the environment is returned here *)
(* TODO: should a pattern also return the substituions? *)
val type_pat : Env.t -> Syntax.term -> (Type.t * Tree.pat) * Env.t
