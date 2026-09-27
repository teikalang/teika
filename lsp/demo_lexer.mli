exception Lexer_error of { location : Location.t; lexeme : string }
exception Parser_error of { location : Location.t }

val term_opt_from_string : string -> Ctree.term option
val tokens_from_string : string -> Cparser.token list
