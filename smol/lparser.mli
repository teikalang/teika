exception Invalid_notation of { loc : Location.t }

val parse_term : loc:Location.t -> Stree.term -> Ltree.term
val parse_pat : loc:Location.t -> Stree.term -> Ltree.pat
