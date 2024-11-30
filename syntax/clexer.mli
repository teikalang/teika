exception Lexer_error of { loc : Location.t }
exception Parser_error of { loc : Location.t }

val loc : Sedlexing.lexbuf -> Location.t
val next : Sedlexing.lexbuf -> Cparser.token * Lexing.position * Lexing.position
val term_opt_from_string : string -> Ctree.term option
