val pp : Format.formatter -> Lparser.Syntax_tree.term -> unit
val to_string : Lparser.Syntax_tree.term -> string
val pp_pattern : Format.formatter -> Lparser.Syntax_tree.pat -> unit
val pattern_to_string : Lparser.Syntax_tree.pat -> string
